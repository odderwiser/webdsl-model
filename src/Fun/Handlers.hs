{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Fun.Handlers where
import Utils.Handler
import Fun.Effects
import Eval.Effects
import Fun.Syntax
import Utils.Environment as U
import Data.List (find)
import Utils.Free

funReturn :: Functor remEff => Handler (Abort val) val remEff val
funReturn = Handler
  { ret = pure
  , hdlr = \(Abort v) -> pure v }

defs :: (Functor eff)
  => Handler_ (MLState FunName (FDecl (FreeEnv eff v)))
  a (Env eff v) eff (a, Env eff v)
defs = mkRHandler U.defs
  (\name -> find (\(FDecl name' _ _) -> name == name' ))
  (\k value@(FDecl name _ _) env ->
    k name $ env { U.defs = value : U.defs env } )

dropH :: (Functor eff) => Handler (MutateEnv (Env eff v)) (Env eff v) eff (Env eff v)
dropH = Handler
  { ret = pure
  , hdlr = \effect -> case effect of
      DropLocalVars env k -> k $ env { varEnv = [] }
      LiftObjectEnv global obj k -> k $ global 
        { varEnv = varEnv obj ++ varEnv global
        , U.defs = U.defs obj ++ U.defs global 
        }
  }
  -- Handler_
  -- { ret_ = \x map -> pure (x, map)
  -- , hdlr_ = \x map -> case (U.defs map, x) of
  --     (env, Deref key k) -> k (case lookup key env of 
  --       Just fun -> fun ) map
  --     (env, Ref val@(FDecl name vars body) k) -> k 
  --       name 
  --       (map { U.defs = (name, val) : U.defs map } )
  -- }

