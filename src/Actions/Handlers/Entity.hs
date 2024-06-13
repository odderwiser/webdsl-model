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
import Data.UUID.V1 (nextUUID)
import Data.UUID (toString)
import Data.UUID.V4 (nextRandom)
import Data.UUID.V3 (generateNamed, namespaceOID)
import Codec.Binary.UTF8.String (encode)
import Actions.Modules.Str.Syntax (projS, LitStr)
import Data.Maybe (fromJust)
import Definitions.GlobalVars.Syntax (getUuid, Uuid)

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

uuidH :: (Functor eff) 
  => Handler (Random String String) val eff val 
uuidH = Handler
  { ret  = pure
  , hdlr = \(Random obj k) -> 
      k $ toString $ generateNamed namespaceOID $ encode obj
  }

-- in the preprocessing, all the objects are written to the databse. 
-- then

--this only updates the entity state
-- pureEntitiesH :: (Functor eff, LitStr <: v) 
--   => Handler_   (Database v) 
--     val [(Address, (Maybe (EntityDecl (Fix v))))] eff val
-- pureEntitiesH = Handler_ 
--   { ret_ = \a b -> pure a -- this should contain "write to db" action
--   , hdlr_ = \eff entities -> case eff of
--       (Ref v@(Just (EDecl name props)) k) ->
--         let address = length entities
--         in k (Just address, Just $ mapId props) ((address, v) : entities)
--       (Ref Nothing k) ->
--         let address = length entities
--         in k (Just address, Nothing) ((address, Nothing) : entities)
--       (Assign ((Just address, Nothing), Just entity) k) ->
--         let entities' = updateList (address, entity) entities   
--         in k  entities'
--       (Deref (Just address, Nothing) k) -> 
--        k (fromJust $ lookup address entities) entities 
--       (Deref (Nothing, Just id) k) -> 
--        k (snd $ fromJust 
--         $ find (\(int, Just (EDecl name props)) -> mapId props == id ) entities) entities 
--   }

eHeapH :: (Functor eff, LitV Uuid <: v) 
  => Handler_ (EHeap v) val [(Uuid, EntityDecl (Fix v))] eff val
eHeapH = Handler_
  { ret_  = \x env -> pure x
  , hdlr_ = \x env -> case x of
      (Deref key k)     -> k (fromJust $ lookup key env) env
      (Ref value k)     -> let id = fromJust $ getUuid value
        in k id ((id, value) : env)
      (Assign record k) -> k (record : env)
  }
-- updateList :: (Eq a, Eq b1) => 
--   (((a, b1), b2), EntityDecl e) 
--   -> [((a, b1), Maybe (EntityDecl e))] -> [(a, Maybe (EntityDecl e))]
updateList _ [] = []
updateList val@(address, entity@(EDecl name props)) ((address', _) : tail) 
  | address == address = (address, Just entity) : tail
  | otherwise = updateList val tail

mapId (("id", val) : tail) = projS val
mapId ((id, _) : tail) = mapId tail
mapId [] = [] 