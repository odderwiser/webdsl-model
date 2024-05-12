module Fun.Handlers where
import Utils.Handler
import Fun.Effects 
import Eval.Effects
import Fun.Syntax
import Utils.Denote as U

funReturn :: Functor remEff => Handler (Abort val) val remEff val
funReturn = Handler
  { ret = pure
  , hdlr = \(Abort v) -> pure v }

defs :: (Functor eff) 
  => Handler_ (MLState FunName (FDecl (FreeEnv eff v))) 
  a (Env eff v) eff (a, Env eff v)
defs = Handler_
  { ret_ = \x map -> pure (x, map)
  , hdlr_ = \x map -> case (U.defs map, x) of
      (env, Deref key k) -> k (case lookup key env of 
        Just fun -> fun ) map
      (env, Ref val@(FDecl name vars body) k) -> k 
        name 
        (map { U.defs = (name, val) : U.defs map } )
  }

