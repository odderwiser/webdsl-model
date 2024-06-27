{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.GlobalVars.TemplatesFramework where
import Utils
import Data.Bifunctor
import Definitions.Program.Syntax
import Definitions.Templates.Framework as T (foldTDefs, handleDefs, foldTProgram, makeTEnv)
import Definitions.GlobalVars.Syntax
import Definitions.Fun.Syntax
import Definitions.Entity.Syntax
import Definitions.Templates.Syntax
import Definitions.GlobalVars.ActionsFramework (EffA)
import Definitions.GlobalVars.Denotation as D
import Definitions.Templates.Denotation
import Definitions.Pages.Denotation as P
import Templates.Effects as E
import Templates.Syntax
import Actions.FrameworkIO hiding (Eff, run)
import Actions.Effects
import Syntax
import Definitions.Entity.Denotation
import Actions.Handlers.Env
import qualified Definitions.Fun.Denotation as F
import qualified Definitions.Entity.Denotation as E
import qualified Definitions.Templates.Denotation as T
import Definitions.Pages.Syntax
import qualified Templates.FrameworkIO as T
import qualified Utils as U
import Templates.Handlers.Env
import Templates.Handlers.Render
import qualified Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Heap (heap, makeEnv)
import Actions.Handlers.Entity (entityDefsH, Elems (..), dbWriteH, eHeapH, uuidH, mockDbReadH, tempEHeapH, tempEHeapH', TempEHeap, DbStatus)
import qualified Templates.Syntax as S
import Actions.Syntax
import Data.Aeson.KeyMap (empty)
import Definitions.GlobalVars.Effects
import Templates.Modules.Layout.Denotation as L
import Templates.Modules.Render.Denotation as X
import Templates.Modules.Page.Denotation as P
import Templates.Modules.Lift.Denotation as Lt
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Cond (condition)
import Templates.Handlers.Forms (singleAccessState, idH)

type Eff = EffA V'
type Eff' = Random Label LabelId + State (Maybe LabelId) 
  + ReqParamsSt + Attribute + Stream HtmlOut + State AttList + E.Render V' + State Address
  + TempEHeap V' + EHeap V' + MLState Address V + DbWrite V + DbRead (EntityDecl V) + End
type Envs = PageDef +: TemplateDef +: LiftT EntityDef +: LiftT FDecl
type Eff'' = PageDefs Eff Eff' V + TDefs Eff Eff'  V + EntityDefsEnv Eff V
    + FunctionEnv Eff V + End
type EnvTy = FreeEnv Eff V 
type T = PageCall +: VarListT +: Layout +: S.Render +: Page +: LiftT Stmt
type DefSyntax = Envs  (BiFix T (Fix Module)) (Fix Module) 

foldProgramVT :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g, VarListT <:: f, PageCall <:: f)
    => ProgramV (Fix h) (g (BiFix f (Fix h)) (Fix h)) (BiFix f (Fix h))
    -> Program (g (PEnv eff eff' v) (FreeEnv eff v )) (PEnv eff eff' v)
foldProgramVT (WithVars vars (Fragment defs program)) = T.foldTProgram $ Fragment
    defs
    (injBf $ VList vars program)

foldProgramVT (WithVars vars (Program defs)) = foldProgramVT
    (WithVars vars (Fragment defs (injBf $ PCall "root" [] [])))

instance DenoteDef FDecl EnvTy Eff'' where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef EnvTy Eff'' where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv Eff Eff' V) EnvTy Eff'' where
  denoteDef' = T.denoteDefT

instance DenoteDef' PageDef (PEnv Eff Eff' V) EnvTy Eff'' where
  denoteDef' = P.denoteDef

runProgram :: Program (Envs (PEnv Eff Eff' V) EnvTy) (PEnv Eff Eff' V) -> FilePath
    -> IO T.Out'
runProgram (Fragment defs pCall) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
  $ handle_ pagesH (TEnv { pages = []} :: TEnv Eff Eff' V )
  $ denoteDefList' defs of
  ((((_, tEnv'), tEnv), env'), env) ->
    run $ pCall $ (T.makeTEnv env' env tEnv) { U.pages = pages tEnv'}

runObservableProgram :: Program (Envs (PEnv Eff Eff' V) EnvTy) (PEnv Eff Eff' V) -> FilePath
    -> IO (T.Out', DbStatus)
runObservableProgram (Fragment defs pCall) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
  $ handle_ pagesH (TEnv { pages = []} :: TEnv Eff Eff' V )
  $ denoteDefList' defs of
  ((((_, tEnv'), tEnv), env'), env) ->
    runObservable $ pCall $ (T.makeTEnv env' env tEnv) { U.pages = pages tEnv'}

run:: Free Eff' () -> FilePath -> IO T.Out'
run e file = do
  (str,  _) <- runObservable e file -- potentially a completely different run function could be used, if the non-emitting Handlers
    -- are faster
  return str

runObservable :: Free Eff' () -> FilePath -> IO (T.Out', DbStatus)
runObservable e file = do
  (((_, str), heap), writeStatus) <- unwrap
    $ handle mockDbReadH
    $ handle_ (dbWriteH file) []
    $ handle_ heap (makeEnv [])
    $ handle_ eHeapH []
    $ handle_ tempEHeapH' (makeEnv [])
    $ handle_ stateElH Nothing
    $ handle renderH
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { R.title = Nothing, Templates.Handlers.Render.body = ""})
    $ handle_ attributeH ("section", 1)
    $ handle_ paramsH mkParamsMap
    $ handle_ singleAccessState Nothing
    $ handle idH
    $ e
  return (str, writeStatus)

instance Lift Eff Eff' V where
  lift  = handleExp



handleExp :: () => Free Eff V -> Free Eff' V
handleExp e = bubbleDown
  $ handle uuidH
  $ handle condition
  $ handle funReturn
  e


-- probably a beeter way to implement this??
bubbleDown ::
    (eff <<: eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance DenoteT Layout Eff Eff' V where
  denoteT = L.denote

instance DenoteT S.Render Eff Eff' V where
  denoteT = X.denote

instance DenoteT Page Eff Eff' V where
  denoteT = P.denote

instance DenoteT (LiftT Stmt) Eff Eff' V where
  denoteT = Lt.denote

instance DenoteT PageCall Eff Eff' V where
  denoteT = denoteP

instance DenoteT VarListT Eff Eff' V where
  denoteT = D.denoteT

-- eDefEnv :: EName -> Props -> [FDecl e] -> EntityDecl e
eDefEnv a b c = Left $ eDef a b c