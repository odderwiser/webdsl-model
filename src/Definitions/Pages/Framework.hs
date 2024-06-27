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
import Actions.FrameworkIO
import Definitions.Pages.Denotation as D (PageDefs, denoteDef)
import Actions.Handlers.Env
import Actions.Handlers.Entity
import Templates.Handlers.Env
import qualified Templates.FrameworkIO as T
import Definitions.Fun.Syntax (FDecl)
import Definitions.Entity.Syntax (EntityDef)
import Definitions.Templates.Syntax (TemplateDef)
import Definitions.Templates.Framework (handleDefs)
import Templates.Modules.Page.Denotation (denoteP)
import Templates.Effects (ReqParamsSt, Attribute, Stream, HtmlOut, State, LabelId, Label)
import Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Heap
import Templates.Modules.Attributes.Syntax (AttList)
import qualified Templates.Effects as E
import Actions.Effects (MLState, Random)
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
import Templates.Handlers.Forms (singleAccessState, idH, autoIncrementState)
import Data.Aeson (ToJSON, FromJSON)
import Actions.Modules.Str.Syntax (LitStr)
import Actions.Arith (LitInt)
import Actions.Bool (LitBool)
import Actions.Values (Null)

type Envs = PageDef +: TemplateDef +: LiftT EntityDef +: LiftT FDecl
type Eff'' v = PageDefs (EffV v) (T.Eff' v) (Fix v) 
  + TDefs (EffV v) (T.Eff' v) (Fix v) 
  + EntityDefsEnv (EffV v) (Fix v) + FunctionEnv (EffV v) (Fix v) + End
type DefSyntax = Envs T.Module' (Fix Module)
type Program' = Program DefSyntax (PageCall T.Module' (Fix Module))
-- type Eff' = State E.Seed + Random Label LabelId + State (Maybe LabelId) + ReqParamsSt + Attribute + Stream HtmlOut
--   + State AttList + E.Render V' + MLState Address V + State Address + End


foldProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g)
    => Program ((g (BiFix f (Fix h)))  (Fix h)) (PageCall (BiFix f (Fix h)) (Fix h))
    -> Program ((g ( PEnv eff eff' v)) (FreeEnv eff v)) (PageCall ( PEnv eff eff' v) (FreeEnv eff v))
foldProgram (Fragment defs pg@(PCall name args params))
    = Fragment (fmap T.foldTDefs defs) (bimap foldDT foldD pg)

foldProgram (Program defs)
    = Fragment (fmap T.foldTDefs defs) (PCall "root" [] []) -- can root have arguments? 

runProgram :: forall v. (ToJSON (v(Fix v)), FromJSON (v (Fix v)), 
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, Null <: v) 
  => Program (Envs (PEnv (EffV v) (T.Eff' v) (Fix v)) (T.EnvTy v)) 
  (PageCall (PEnv (EffV v) (T.Eff' v) (Fix v)) (T.EnvTy v)) -> String -> IO T.Out'
runProgram (Fragment defs pCall) file = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env (EffV v) (Fix v) )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (EffV v) (Fix v) )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv (EffV v) (T.Eff' v) (Fix v) )
  $ handle_ pagesH (TEnv { pages = []} :: TEnv (EffV v) (T.Eff' v) (Fix v) )
  $ denoteDefList' defs of
  ((((_, tEnv'), tEnv), env'), env) ->
    T.runApplied
    (denoteP pCall
    $ ((makeTEnv env' env tEnv) :: TEnv (EffV v) (T.Eff' v) (Fix v)) { U.pages = pages tEnv'}) file 

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


test exp env env' tEnv tEnv' = denoteP exp
  $ (T.makeTEnv env' env tEnv) { U.pages = pages tEnv'}

instance DenoteDef' PageDef (PEnv (EffV v) (T.Eff' v) (Fix v)) (T.EnvTy v) (Eff'' v) where
  denoteDef' = D.denoteDef

instance DenoteDef FDecl (T.EnvTy v) (Eff'' v) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (T.EnvTy v) (Eff'' v) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV v) (T.Eff' v) (Fix v)) (T.EnvTy v) (Eff'' v) where
  denoteDef'= T.denoteDefT

pDefEnv a b c = Right $ pDef a b c