module Entity.Handlers where
import Eval.Effects
import Entity.Syntax
import Syntax
import Utils.Handler
import Utils.Environment as U
import Fun.Syntax
import Utils.Environment (Function, Env)

entity :: (Functor g) => Handler_ (MLState (PName, ScopedType) Address) 
  a (EntityEnv v) g (a, EntityEnv v)
entity = Handler_
  { ret_ = \x map -> pure (x, map)
  , hdlr_ = \effectCon env -> case (env, effectCon) of
      (EEnv _ eenv, Deref key k) -> k (case lookup key eenv of Just x -> x) env
      (_, Assign record k) -> k $ insertEnv record env
  }

insertEnv record (EEnv name env) = EEnv name (record : env)

eDefs :: (Functor eff) 
  => Handler_ (MLState Address (Function eff v)) 
  a (Env eff v) eff (a, Env eff v)
eDefs = mkRHandler U.eDefs lookup 
  (\ k val@(FDecl name vars body) env ->
    let address = length $ U.eDefs env in 
      k address
      $ env { U.eDefs = (address, val) : U.eDefs env })

--  = Handler_
--   { ret_ = \x map -> pure (x, map)
--   , hdlr_ = \x map -> case (U.eDefs map, x) of
--       (env, Deref key k) -> k (case lookup key env of 
--         Just fun -> fun ) map
--       (env, Ref val@(FDecl name vars body) k) -> 
--         let address = length env in 
--         k 
--         (length env)
--         (map { U.eDefs = (address, val) : U.eDefs map } )
--   }
