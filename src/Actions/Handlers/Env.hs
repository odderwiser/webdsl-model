module Actions.Handlers.Env where

import Utils.Handler
import Actions.Effects
import Actions.Modules.Fun.Syntax
import Utils.Environment as U
import Data.List (find)
import Utils.Free
import Data.Maybe (fromJust)
import Definitions.Fun.Syntax

type FunctionEnv eff v = MLState FunName (FDecl (FreeEnv eff v))

defsH :: (Functor eff, Functor eff')
  => Handler_ (FunctionEnv eff v)
  a (Env eff v) eff' (a, Env eff v)
defsH = mkRHandler U.defs
  (\name -> find (\(FDecl name' _ _) -> name == name' ))
  (\k value@(FDecl name _ _) env ->
    k name $ env { U.defs = value : U.defs env } )

scopeDefs :: (Functor eff)
  => Handler_ (MLState FunName (FDecl (FreeEnv eff v)))
  a (Env eff v) eff (a, Env eff v)
scopeDefs = defsH

dropH :: (Functor eff) => Handler (DropEnv (Env eff' v)) (Env eff' v) eff (Env eff' v)
dropH = Handler
  { ret = pure
  , hdlr = \(DropLocalVars env k) -> k $ dropAction env
      -- LiftObjectEnv global obj k -> k $ global 
      --   { varEnv = varEnv obj ++ varEnv global
      --   , U.defs = U.defs obj ++ U.defs global 
      --   }
  }

dropAction env = env { varEnv = [] }

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

mkAHandler' envSubtype finder setter = Handler_
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

refH :: (Functor remEff)
  => b -> Handler_ (MLState a b) a env remEff (a, env)
  -> env -> Free remEff (a, env)
refH value handler env = handle_ handler env (ref value)