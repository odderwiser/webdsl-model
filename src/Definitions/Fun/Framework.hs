{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.Fun.Framework where
import Definitions.Fun.Denotation as F
import Actions.Framework
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Utils as U
import Definitions.Program.Syntax

type Envs = FDecl
type Eff' v = FunctionEnv (EffV v) (Fix v) + End

instance DenoteDef FDecl (FreeEnv (EffV v)  (Fix v)) (Eff' v) where
  denoteDef = F.denoteDef

runProgram :: Program (Envs (FreeEnv Eff V)) () (FreeEnv Eff V)
  -> Out
runProgram (Fragment defs _ exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], U.defs =[]} :: Env Eff V ) 
  $ denoteDefList defs of
    (_, env) -> run exp env []

    
