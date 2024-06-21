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
import Actions.Framework hiding (run)
import Actions.Effects
import Syntax
import Definitions.Entity.Denotation
import Actions.Handlers.Env
import qualified Definitions.Fun.Denotation as F
import qualified Definitions.Entity.Denotation as E
import qualified Definitions.Templates.Denotation as T
import Definitions.Pages.Syntax
import qualified Templates.Framework as T
import qualified Utils as U
import Templates.Handlers.Env
import Templates.Handlers.Render
import qualified Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Heap (heap, makeEnv)
import Actions.Handlers.Entity (entityDefsH, Elems (..), dbWriteH, eHeapH, uuidH, mockDbReadH, tempEHeapH, tempEHeapH', TempEHeap)
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

type Eff' = Random Label LabelId + State (Maybe LabelId) 
  + ReqParamsSt + Attribute + Stream HtmlOut + State AttList + E.Render V' + State Address
  + DbRead (EntityDecl V) + TempEHeap V' + EHeap V' + MLState Address V + DbWrite (EntityDecl V) + End
type Envs = PageDef +: TemplateDef +: LiftT EntityDef +: LiftT FDecl
type Eff'' = PageDefs EffA Eff' V + TDefs EffA Eff' V + EntityDefsEnv EffA V
    + FunctionEnv EffA V + End
type EnvTy = FreeEnv EffA V 
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

instance DenoteDef' TemplateDef (PEnv EffA Eff' V) EnvTy Eff'' where
  denoteDef' = T.denoteDefT

instance DenoteDef' PageDef (PEnv EffA Eff' V) EnvTy Eff'' where
  denoteDef' = P.denoteDef

runProgram :: Program (Envs (PEnv EffA Eff' V) EnvTy) (PEnv EffA Eff' V) -> FilePath
    -> IO T.Out'
runProgram (Fragment defs pCall) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env EffA V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env EffA V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv EffA Eff' V )
  $ handle_ pagesH (TEnv { pages = []} :: TEnv EffA Eff' V )
  $ denoteDefList' defs of
  ((((_, tEnv'), tEnv), env'), env) ->
    run $ pCall $ (T.makeTEnv env' env tEnv) { U.pages = pages tEnv'}

run:: Free Eff' () -> FilePath -> IO T.Out'
run e file = do
  ((_, str), _) <- unwrap
    $ handle_ (dbWriteH file) (Elems {vars = empty, entities = empty, classes = empty} :: Elems V')
    $ handle_ heap (makeEnv [])
    $ handle_ eHeapH []
    $ handle_ tempEHeapH' (makeEnv [])
    $ handle mockDbReadH
    $ handle_ stateElH Nothing
    $ handle renderH
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { R.title = Nothing, body = ""})
    $ handle_ attributeH ("section", 1)
    $ handle_ paramsH mkParamsMap
    $ handle_ singleAccessState Nothing
    $ handle idH
    $ e
  return str

instance Lift EffA Eff' V where
  lift  = handleExp



handleExp :: () => Free EffA V -> Free Eff' V
handleExp e = bubbleDown
  $ handle uuidH
  $ handle condition
  $ handle mockDbReadH
  $ handle funReturn
  e


-- probably a beeter way to implement this??
bubbleDown ::
    (eff <<: eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance DenoteT Layout EffA Eff' V where
  denoteT = L.denote

instance DenoteT S.Render EffA Eff' V where
  denoteT = X.denote

instance DenoteT Page EffA Eff' V where
  denoteT = P.denote

instance DenoteT (LiftT Stmt) EffA Eff' V where
  denoteT = Lt.denote

instance DenoteT PageCall EffA Eff' V where
  denoteT = denoteP

instance DenoteT VarListT EffA Eff' V where
  denoteT = D.denoteT

-- eDefEnv :: EName -> Props -> [FDecl e] -> EntityDecl e
eDefEnv a b c = Left $ eDef a b c