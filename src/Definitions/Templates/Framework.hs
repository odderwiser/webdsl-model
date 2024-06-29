{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Templates.Framework where

import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import qualified Definitions.Entity.Framework as E
import Definitions.Fun.Denotation as F
import Definitions.Templates.Denotation as T
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Actions.Handlers.Return (funReturn)
import Actions.FrameworkIO as A
import Definitions.Templates.Syntax (TemplateDef, tDef)
import Templates.FrameworkIO as T
import Templates.Handlers.Env (templatesH)
import qualified Data.Bifunctor
import Data.Bifunctor
import Templates.Syntax (LiftT, EvalT)
import Templates.Modules.Lift.Syntax (LiftT(..))
import Actions.Str (LitStr)
import Actions.Arith (LitInt)
import Actions.Bool (LitBool)
import Data.Aeson (ToJSON, FromJSON)
import Templates.Modules.Lift.Syntax (LiftE)
--running syntax

-- preprocessing
type Envs = TemplateDef +: LiftE EntityDef +: LiftE FDecl
type Eff'' v = TDefs (EffV v) (Eff' v) (Fix v) + EntityDefsEnv (EffV v) (Fix v) + FunctionEnv (EffV v) (Fix v) + End
type EnvTy v = FreeEnv (EffV v) (Fix v)
type DefSyntax = Envs Module' (Fix Module)

runProgram :: (LitStr <: v,  LitInt <: v, LitBool <: v, [] <: v, 
  ToJSON (v(Fix v)), FromJSON (v (Fix v)), Show (v (Fix v)))
  => Program (Envs (PEnv (EffV v) (Eff' v) (Fix v)) (EnvTy v)) ()
  (PEnv (EffV v) (Eff' v) (Fix v)) -> String -> IO Out'
runProgram (Fragment defs Nothing exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env (EffV v) (Fix v) )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (EffV v) (Fix v) )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv (EffV v) (Eff' v) (Fix v) )
  $ denoteDefList' defs of
  (((_, tEnv), env'), env) -> T.runEnv exp
    $ makeTEnv env' env tEnv

handleDefs defs = handle_
  defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (EffV v) (Fix v) )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv (EffV v) (Eff' v) (Fix v) )
  defs

makeTEnv :: Env eff v -> Env eff v -> TEnv eff eff' v -> TEnv eff eff' v
makeTEnv eEnv fEnv tEnv = TEnv
    { actionEnv = Env
      { varEnv = []
      , entityDefs = entityDefs eEnv
      , defs = U.defs  fEnv
      , globalVars = []
      }
    , templates = templates tEnv
    , elements = []
    }

foldTProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g)
    => Program ((g (BiFix f (Fix h))) (Fix h)) () (BiFix f (Fix h))
    -> Program ((g (PEnv eff eff' v)) (FreeEnv eff v)) () (PEnv eff eff' v)
foldTProgram (Fragment defs _ program) = Fragment (fmap foldTDefs defs) Nothing (foldDT program)

foldTDefs :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g)
  => (g (BiFix f (Fix h))) (Fix h) -> (g (PEnv eff eff' v)) (FreeEnv eff v)
foldTDefs = bimap foldDT foldD 

instance DenoteDef FDecl (EnvTy v) (Eff'' v) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (EnvTy v) (Eff'' v) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV v) (Eff' v) (Fix v)) (EnvTy v)  (Eff'' v) where
  denoteDef' = T.denoteDefT


tDefEnv a b c = Right $ tDef a b c