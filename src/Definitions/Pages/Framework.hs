{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.Pages.Framework where
import Utils as U
import Data.Bifunctor
import Definitions.Program.Syntax
import Definitions.Pages.Syntax
import qualified Definitions.Templates.Framework as T
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Templates.Denotation as T
import Actions.Framework
import Definitions.Pages.Denotation as D (PageDefs, denoteDef)
import Actions.Handlers.Env
import Actions.Handlers.Entity
import Templates.Handlers.Env
import qualified Templates.Framework as T
import Definitions.Fun.Syntax (FDecl)
import Definitions.Entity.Syntax (EntityDef)
import Definitions.Templates.Syntax (TemplateDef)
import Definitions.Templates.Framework (handleDefs)
import Templates.Modules.Page.Denotation (denoteP)
import Templates.Effects (ReqParamsSt, Attribute, Stream, HtmlOut, State)
import Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Heap
import Templates.Modules.Attributes.Syntax (AttList)
import qualified Templates.Effects as E
import Actions.Effects (MLState)
import Syntax (Address)
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Cond (condition)
import Templates.Syntax as S
import Actions.Syntax
import qualified Templates.Modules.Layout.Denotation as L
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Page.Denotation as P
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Forms.Denotation as F

type Envs = PageDef + TemplateDef + EntityDef + FDecl
type Eff'' = PageDefs Eff Eff' V + TDefs Eff Eff' V + EntityDefsEnv Eff V + FunctionEnv Eff V + End
type EnvTy = (FreeEnv Eff V \/ PEnv Eff Eff' V)
type DefSyntax = Envs (Fix Module) \/ Envs T.Module'
type Program' = Program DefSyntax (PageCall (Fix Module) T.Module')
type Eff' = ReqParamsSt + Attribute + Stream HtmlOut 
  + State AttList + E.Render V' + MLState Address V + State Address + End


foldProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Functor g)
    => Program (g (Fix h) \/ g (BiFix f (Fix h))) (PageCall (Fix h) (BiFix f (Fix h)))
    -> Program (g (FreeEnv eff v \/ PEnv eff eff' v)) (PageCall (FreeEnv eff v) ( PEnv eff eff' v))
foldProgram (Fragment defs pg@(PCall name args params)) 
    = Fragment (fmap T.foldTDefs defs) (bimap foldD foldDT pg)

foldProgram (Program defs) 
    = Fragment (fmap T.foldTDefs defs) (PCall "root" [] []) -- can root have arguments? 

runProgram :: Program (Envs EnvTy) (PageCall (FreeEnv Eff V) (PEnv Eff Eff' V)) -> T.Out'
runProgram (Fragment defs pCall) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
  $ handle_ pagesH (TEnv { pages = []} :: TEnv Eff Eff' V )
  $ denoteDefList defs of
  ((((_, tEnv'), tEnv), env'), env) -> 
    runApplied
    $ denoteP pCall
    $ ((makeTEnv env' env tEnv) :: TEnv Eff Eff' V) { U.pages = pages tEnv'} 

makeTEnv :: Env eff v -> Env eff v -> TEnv eff eff' v -> TEnv eff eff' v
makeTEnv eEnv fEnv tEnv = TEnv 
    { actionEnv = Env
      { varEnv = []
      , entityDefs = entityDefs eEnv
      , defs = U.defs  fEnv
      , globalVars = []
      }
    , templates = templates tEnv
    , U.elements = []
    }

runApplied :: Free Eff' () -> T.Out'
runApplied e = case unwrap
    $ handle_ stateElH Nothing
    $ handle_ heap (makeEnv [])
    $ handle renderH
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { R.title = Nothing, body = "", pageCall = False})
    $ handle_ attributeH ("section", 1)
    $ handle_ paramsH mkParamsMap
    $ e 
  of
    ((_, str), heap)    -> str

test exp env env' tEnv tEnv' = denoteP exp
  $ (T.makeTEnv env' env tEnv) { U.pages = pages tEnv'} 

instance DenoteDef PageDef EnvTy Eff'' where
  denoteDef decl = D.denoteDef $ fmap (\(Right d) -> d) decl

instance DenoteDef FDecl EnvTy Eff'' where --- Maybe?? This works???
  denoteDef decl = F.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef EntityDef EnvTy Eff'' where
  denoteDef decl = E.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef TemplateDef EnvTy Eff'' where
  denoteDef decl = T.denoteDefT $ (fmap (\(Right d) -> d) decl :: TemplateDef (PEnv Eff Eff' V))


handleExp :: () => Free Eff V -> Free Eff' V
handleExp e = bubbleDown
  $ handle_ eHeapH [] -- probably wrong?
  $ handle uuidH
  $ handle funReturn
  $ handle condition
  e
    

-- probably a beeter way to implement this??
bubbleDown ::
    (eff <<: eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance Lift Eff Eff' V where
  lift = handleExp

instance DenoteT Layout Eff Eff' V where
  denoteT :: Layout (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = L.denote

instance DenoteT S.Render Eff Eff' V where
  denoteT :: S.Render (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = X.denote

instance DenoteT Page Eff Eff' V where
  denoteT = P.denote

instance DenoteT (LiftT Stmt) Eff Eff' V where
  denoteT = Lt.denote

instance DenoteT Forms Eff Eff' V where
  denoteT = F.denoteR

pDefEnv a b c = Right $ pDef a b c 