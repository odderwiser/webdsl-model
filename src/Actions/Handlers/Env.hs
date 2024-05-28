module Actions.Handlers.Env where

import Utils.Handler
import Fun.Effects
import Eval.Effects
import Fun.Syntax
import Utils.Environment as U
import Data.List (find)
import Utils.Free

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

dropH :: (Functor eff) => Handler (DropEnv (Env eff v)) (Env eff v) eff (Env eff v)
dropH = Handler
  { ret = pure
  , hdlr = \(DropLocalVars env k) -> k $ dropAction env
      -- LiftObjectEnv global obj k -> k $ global 
      --   { varEnv = varEnv obj ++ varEnv global
      --   , U.defs = U.defs obj ++ U.defs global 
      --   }
  }

dropAction env = env { varEnv = [] }