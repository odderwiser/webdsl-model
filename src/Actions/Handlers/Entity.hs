{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Actions.Handlers.Entity where
import Actions.Effects
import Actions.Modules.Entity.Syntax
import Syntax
import Utils as U
import Actions.Effects
import qualified Actions.Modules.Bool.Syntax as B
import qualified Actions.Modules.Arith.Syntax as A
import qualified Syntax as S
import Actions.Handlers.Env (dropAction, mkAHandler, mkRHandler)
import Data.List (find)
import Definitions.Entity.Denotation
import Definitions.Entity.Syntax

-- import Fun.Syntax
-- import Utils.Environment (Function, Env)
-- import Utils.Free
-- import Utils.Composition
-- import Data.Maybe (mapMaybe)
-- import Entity.Effects (Write(..), write, MutateEnv (..), DefaultValue (DefaultValue))
-- import Data.Foldable (find)
-- import Fun.Effects
-- import Fun.Handlers (dropAction)
-- import qualified Actions.Modules.Arith.Syntax as A
-- import Utils.Fix
-- import qualified Actions.Modules.Bool.Syntax as B
-- import Syntax as S

entityDefsH :: (Functor eff, Functor eff')
  => Handler_ (EntityDefsEnv eff v)
  a (Env eff v) eff' (a, Env eff v)
entityDefsH = mkRHandler U.entityDefs
  (\name -> find (\(EDef name' _ _ _) -> name == name' ))
  (\k val@(EDef name _ _ _) env -> k name
    $ env { U.entityDefs = val : U.entityDefs env  }
  )

scopedEntityDefsH :: Functor eff 
  => Handler_ (EntityDefsEnv eff v)
  a (Env eff v) eff (a, Env eff v)
scopedEntityDefsH = entityDefsH

defaultTypeH :: (Functor eff, A.LitInt <: v, B.LitBool <: v,
  [] <: v, Null <: v) 
  => Handler (DefaultValue (Fix v)) 
  (Fix v) eff (Fix v)
defaultTypeH = Handler
  { ret = pure
  , hdlr = \(DefaultValue ty k) -> case ty of
      Int -> k $ injF $ A.Lit 0
      Bool -> k $ injF $ B.Lit False
      List -> k $ injF $ []
      S.Entity -> k $ injF $ Null
  }

mutateH :: (Functor eff) 
  => Handler (MutateEnv (Env eff v)) 
  (Env eff v) eff (Env eff v)
mutateH = Handler
  { ret = pure
  , hdlr = \effect -> case effect of
      Drop (DropLocalVars env k) -> k $ dropAction env
      LiftObjectEnv global obj k -> k $ global 
        {U.defs = U.defs obj ++ U.defs global }
      -- GenerateEmptyEnv k -> k 
      --   $ Env { varEnv = [], defs = []}
      -- GenerateDefaultEnv (EDef name props funs) k -> 
  }

objEnvH :: (Functor eff) 
  => Handler_   (MLState PName Address) val 
  [(PName, Address)] eff (val, [(PName, Address)])
objEnvH = mkAHandler id lookup (:)

propertyVarEnvH ::(Functor eff) 
  => Handler_   (MLState PName Address) val 
    (Env eff v) eff (val, Env eff v)
propertyVarEnvH = mkAHandler U.objVarEnv lookup 
  (\record env -> env { U.objVarEnv = record : U.objVarEnv env})