{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Templates.Framework where

import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Templates.Denotation as T
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Actions.Handlers.Return (funReturn)
import Actions.Framework as A
import Definitions.Templates.Syntax (TemplateDef, tDef)
import Templates.Framework as T
import Templates.Handlers.Env (templatesH)
import qualified Data.Bifunctor
import Data.Bifunctor
import Templates.Syntax (LiftT, EvalT)
import Templates.Modules.Lift.Syntax (LiftT(..))
--running syntax

-- preprocessing
type Envs = TemplateDef +: LiftT EntityDef +: LiftT FDecl
type Eff'' = TDefs Eff Eff' V + EntityDefsEnv Eff V + FunctionEnv Eff V + End
type EnvTy = FreeEnv Eff V
type DefSyntax = Envs Module' (Fix Module)

runProgram :: Program (Envs (PEnv Eff Eff' V) EnvTy) (PEnv Eff Eff' V) -> Out'
runProgram (Fragment defs exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
  $ denoteDefList' defs of
  (((_, tEnv), env'), env) -> T.runEnv exp
    $ makeTEnv env' env tEnv

handleDefs defs = handle_
  defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
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
    => Program ((g (BiFix f (Fix h))) (Fix h)) (BiFix f (Fix h))
    -> Program ((g (PEnv eff eff' v)) (FreeEnv eff v)) (PEnv eff eff' v)
foldTProgram (Fragment defs program) = Fragment (fmap foldTDefs defs) (foldDT program)

foldTDefs :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g)
  => (g (BiFix f (Fix h))) (Fix h) -> (g (PEnv eff eff' v)) (FreeEnv eff v)
foldTDefs = bimap foldDT foldD 

instance DenoteDef FDecl EnvTy Eff'' where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef EnvTy Eff'' where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv Eff Eff' V) EnvTy  Eff'' where
  denoteDef' = T.denoteDefT


tDefEnv a b c = Right $ tDef a b c