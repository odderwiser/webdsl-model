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
import Actions.Modules.Str.Syntax 
import Data.Maybe (fromJust)
import Definitions.GlobalVars.Syntax (getUuid, Uuid)
import Definitions.GlobalVars.Effects
import Actions.Modules.Eval.Syntax (VName)
import Data.Aeson.KeyMap (KeyMap, insert)
import Data.Aeson.Key (fromString)
import qualified Data.Aeson as D
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Actions.Values


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
      Int      -> k $ box (0 :: Int)
      Bool     -> k $ box False
      List     -> k $ injF []
      S.Entity -> k $ injF Null
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

eHeapH :: (Functor eff, Lit Uuid <: v)
  => Handler_ (EHeap v) val [(Uuid, EntityDecl (Fix v))] eff val
eHeapH = Handler_
  { ret_  = \x env -> pure x
  , hdlr_ = \x env -> case x of
      (Deref key k)     -> k (fromJust $ Prelude.lookup key env) env
      (Ref value k)     -> let id = getUuid value
        in k id ((id, value) : env)
      (Assign record k) -> k (record : env)
  }

mockDbReadH :: (Functor remEff)
  => Handler (DbRead (EntityDecl v)) val remEff val
mockDbReadH = Handler
  { ret  = pure
  , hdlr = \(Connect k ) -> k False 
    -- technically, the other effects shouldn't occur
  }


data Elems v = Elems (KeyMap Uuid) (KeyMap (EntityDecl (Fix v)))
  deriving Generic

instance (ToJSON (v (Fix v))) => ToJSON (Elems v)
instance (ToJSON v) => ToJSON (EntityDecl v)
instance (ToJSON (v (Fix v))) => ToJSON (Fix v)

-- dbWriteH :: forall remEff val v. 
--   (Functor remEff, Lit Uuid <: v, (ToJSON (v (Fix v))))
--   => String -> Handler_ (DbWrite (EntityDecl (Fix v))) val (Elems v) remEff (IO val)
-- dbWriteH dbEntry = Handler_ 
--   { ret_ = \ val elems ->
--        let decoded = D.encode elems
--        in _
--   , hdlr_ = \eff (Elems vars decls) -> case eff of 
--     (SetVar (name, id) k) -> k 
--       $ Elems (insert (fromString name) id vars) decls
--     (SetEntity e k) -> k 
--       $ Elems vars (insert (fromString $ getUuid e) e decls)
--   }