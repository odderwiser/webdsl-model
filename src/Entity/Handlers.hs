{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# OPTIONS_GHC -Wno-missing-fields #-}
module Entity.Handlers where
import Eval.Effects
import Entity.Syntax
import Syntax
import Utils.Handler
import Utils.Environment as U
import Fun.Syntax
import Utils.Environment (Function, Env)
import Utils.Free
import Utils.Composition
import Data.Maybe (mapMaybe)
import Entity.Effects (Write(..), write, MutateEnv (..), DefaultValue (DefaultValue))
import Data.Foldable (find)
import Fun.Effects
import Fun.Handlers (dropAction)
import qualified Arith.Syntax as A
import Utils.Fix
import qualified Bool.Syntax as B
import Syntax as S

type EntityDefsEnv eff v = (MLState EName (EntityDef (FreeEnv eff v)))

entityDefsH :: (Functor eff, Functor eff')
  => Handler_ (EntityDefsEnv eff v)
  a (Env eff v) eff' (a, Env eff v)
entityDefsH = mkRHandler U.entityDefs
  (\name -> find (\(EDef name' _ _) -> name == name' ))
  (\k val@(EDef name props funs) env -> k name
    $ env { U.entityDefs = val : U.entityDefs env  }
  )

scopedEntityDefsH :: Functor eff 
  => Handler_ (EntityDefsEnv eff v)
  a (Env eff v) eff (a, Env eff v)
scopedEntityDefsH = entityDefsH
-- refEntities :: forall eff g v eDef. (Functor eff, EntityDef <: g,
--     eDef ~ EntityDef (FreeEnv eff v))
--     => [g (FreeEnv eff v)] -> Env eff v
--     -> Free eff (Env eff v)
-- refEntities entities env  = do
--   (_ :: [EName], env') <- handle_ entityDefsH env 
--     $ mapM ref
--     $ mapMaybe (\dec -> (proj dec :: Maybe eDef)) entities
--   return env'

-- entityDeclsH :: Functor eff 
--   => Handler_ (MLState Address (EName, Env eff v))
--   a (Env eff v) eff (a, Env eff v)
-- entityDeclsH = mkRHandler U.entities
--   lookup
--   (\k val env -> 
--     let loc = length (U.entities env) in
--       k loc $ env { U.entities = (loc, val) : U.entities env})

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