{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.Entity.Framework where
import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F

import qualified Definitions.Fun.Framework as F
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Actions.Handlers.Return (funReturn)
import Actions.FrameworkIO
--running syntax

--preprocessing
type Envs = EntityDef + FDecl
type Eff' v = EntityDefsEnv (EffV v) (Fix v) + FunctionEnv (EffV v) (Fix v) + End -- weird

runProgram (Fragment defs exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env (EffV V') V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (EffV V') V ) 
  $ denoteDefList defs of
    ((_, env'), env) -> run exp Env 
      { varEnv = []
      , entityDefs = entityDefs env'
      , defs = U.defs env  
      } []


instance DenoteDef EntityDef (FreeEnv (EffV v) (Fix v)) (Eff' v) where
  denoteDef = E.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV v)  (Fix v)) (Eff' v) where
  denoteDef = F.denoteDef


  -- indexed family to model datatypes
  