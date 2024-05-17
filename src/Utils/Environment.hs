module Utils.Environment where
import Utils.Handler
import Eval.Effects
import Fun.Syntax
import Utils.Free
import Eval.Syntax
import Syntax
import Data.Maybe (fromJust)

type Function eff v = FDecl (Env eff v -> Free eff v)
type FreeEnv eff v = Env eff v -> Free eff v


data Env eff v = Env
    { varEnv   :: [(VName, Address)]
    , defs     :: [Function eff v]
    , eDefs    :: [(Address, Function eff v)]
    }

genericEnvHandler :: (Functor remEff, Eq a)
  => (t -> [(a, v)]) -> ((a, v) -> t -> t) -> Handler_ (MLState a v) b t remEff (b, t)
genericEnvHandler envSubtype setter = Handler_
  { ret_  = curry pure
  , hdlr_ = \x env -> case (envSubtype env, x) of
      (env', Deref key k)     -> k (case lookup key env' of Just x -> x) env
      (env', Assign record k) -> k $ setter record env}

-- mkEnvHandler :: (Env eff v -> env) -> (k -> env -> Maybe v) 
--   -> (k -> v' -> Env eff v -> Free remEff (val, Env eff v)) 
--   -> Bool -> Handler_ (MLState t v) val t remEff (val, t)
mkAHandler envSubtype finder setter = Handler_
  { ret_  = curry pure
  , hdlr_ = \x env -> case (envSubtype env, x) of
      (env', Deref key k)     -> k (fromJust $ finder key env') env
      (env', Assign record k) -> k $ setter record env}

mkRHandler envSubtype finder cont = Handler_
  { ret_  = curry pure
  , hdlr_ = \x env -> case (envSubtype env, x) of
      (env', Deref key k) -> k (fromJust $  finder key env') env
      (env', Ref value k) -> cont k value env
  }
  -- , hdlr_ = \x map -> case (U.defs map, x) of
  --     (env, Deref key k) -> k (case lookup key env of 
  --       Just fun -> fun ) map
  --     (env, Ref val@(FDecl name vars body) k) -> k 
  --       name 
  --       (map { U.defs = (name, val) : U.defs map } )
  -- }