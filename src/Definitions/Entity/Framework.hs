{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.Entity.Framework where
import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Actions.Handlers.Return (funReturn)
import Actions.Framework
--running syntax

--preprocessing
type Envs = EntityDef + FDecl
type Eff' = EntityDefsEnv Eff V + FunctionEnv Eff V + End

runProgram (Fragment defs exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V ) 
  $ denoteDefList defs of
    ((_, env'), env) -> run exp Env 
      { varEnv = []
      , entityDefs = entityDefs env'
      , defs = U.defs env  
      } []


instance DenoteDef FDecl (FreeEnv Eff V) Eff' where
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv Eff V) Eff' where
  denoteDef = E.denoteDef


  -- indexed family to model datatypes
  