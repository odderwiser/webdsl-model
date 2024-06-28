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

runProgram (Fragment defs _ exp) = run exp (handleDefs defs) []

handleDefs :: (DenoteDef sym e (EntityDefsEnv eff v + (FunctionEnv eff v + End)),  
  Functor eff) => [sym e] -> Env eff v
handleDefs defs = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env eff v )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (eff) v ) 
  $ denoteDefList defs
  of 
    ((_, env'), env) -> Env 
      { varEnv = []
      , entityDefs = entityDefs env'
      , defs = U.defs env  
      }

instance DenoteDef EntityDef (FreeEnv (EffV v) (Fix v)) (Eff' v) where
  denoteDef = E.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV v)  (Fix v)) (Eff' v) where
  denoteDef = F.denoteDef


  -- indexed family to model datatypes
  