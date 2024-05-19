module Utils.Environment where
import Utils.Handler
import Eval.Effects
import Fun.Syntax
import Utils.Free
import Eval.Syntax
import Syntax
import Data.Maybe (fromJust)
import Entity.Syntax

type Function eff v = FDecl (Env eff v -> Free eff v)
type FreeEnv eff v = Env eff v -> Free eff v


data Env eff v = Env
    { varEnv     :: [(VName, Address)] -- this is for variables
    , defs       :: [Function eff v] -- this is for functions
    , entityDefs :: [EntityDef (FreeEnv eff v)] -- this is for all the entity definitions
    , entities   :: [(Address, EntityDecl (Env eff v))] -- this is for entity environments (declarations) (name collision wit variables)
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
  
  
derefH :: (Functor eff)
  => a -> Handler_ (MLState a b) b env eff (b, env) 
  -> env -> Free eff b
derefH key handler env = do
  (loc, env) <- handle_ handler env (deref key)
  return loc