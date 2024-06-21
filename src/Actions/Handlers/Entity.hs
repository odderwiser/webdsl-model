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
import Actions.Handlers.Env (dropAction, mkAHandler, mkRHandler, mkAHandler')
import Data.List (find)
import Definitions.Entity.Denotation
import Definitions.Entity.Syntax
import Data.UUID (toString)
import Data.UUID.V3 (generateNamed, namespaceOID)
import qualified Codec.Binary.UTF8.String as S (encode, decode)
import Actions.Modules.Str.Syntax
import Data.Maybe (fromJust)
import Definitions.GlobalVars.Syntax (getUuid, Uuid)
import Definitions.GlobalVars.Effects
import Actions.Modules.Eval.Syntax (VName)
import Data.Aeson.KeyMap (KeyMap, insert, union, insertWith, unionWith)
import Data.Aeson.Key (fromString)
import GHC.Generics (Generic)
import Data.Aeson ( ToJSON, FromJSON)
import qualified Data.Aeson as A (decode, encode)
import Actions.Values
import System.IO (readFile')
import Data.ByteString.Lazy (pack, unpack)
import Actions.Handlers.Heap (heap', heap'')
import qualified Data.Map as Map
import Actions.Modules.Arith.Syntax (boxI)
import System.Directory (doesFileExist)
import qualified Data.Set as Set (Set, union, singleton)
import Data.Map (Map)


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
      Int      -> k $ boxI (0 :: Int)
      Bool     -> k $ boxV False
      List     -> k $ injF []
      S.Entity s -> k $ injF Null
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
      k $ toString $ generateNamed namespaceOID $ S.encode obj
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


data Elems v = Elems
  { vars     :: KeyMap Uuid
  , entities :: KeyMap (EntityDecl (Fix v))
  , classes  :: KeyMap (Set.Set Uuid)
  }
  deriving Generic

instance (FromJSON (v (Fix v))) => FromJSON (Elems v)
instance (ToJSON (v (Fix v))) => ToJSON (Elems v)
instance (ToJSON v) => ToJSON (EntityDecl v)
instance (FromJSON v) => FromJSON (EntityDecl v)
instance (ToJSON (v (Fix v))) => ToJSON (Fix v)
instance (FromJSON (v (Fix v))) => FromJSON (Fix v)

dbWriteH :: forall remEff val v.
  (Functor remEff, Lit Uuid <: v,ToJSON (v (Fix v)), FromJSON (v (Fix v)))
  => FilePath -> Handler_ (DbWrite (EntityDecl (Fix v))) val (Elems v) remEff (IO val)
dbWriteH dbEntry = Handler_
  { ret_ = \ val elems -> pure $ do
    fileExists <- doesFileExist dbEntry
    oldDbState <- if fileExists then readFile' dbEntry else return ""
    case oldDbState of
      "" -> writeFile dbEntry $ encodeElems elems
      _  -> case decodeElems oldDbState of
        Nothing -> writeFile dbEntry $ encodeElems elems
        (Just (Elems vars' objects' classes' :: Elems v)) -> do
          writeFile dbEntry
            $ encodeElems
            $ Elems
              (update vars' (vars elems))
              (update objects' (entities elems))
              (unionWith Set.union classes' (classes elems))
    return val
  , hdlr_ = \eff db@(Elems vars decls classes) -> case eff of
    (SetVar (name, id) k) -> k
      $ Elems (insert (fromString name) id vars) decls classes
    (SetEntity e k) -> k
      $ updateEntity e db
  }
update :: KeyMap a -> KeyMap a -> KeyMap a
update oldMap newMap = union newMap oldMap

encodeElems :: (ToJSON (v (Fix v))) => Elems v -> String
encodeElems = S.decode . unpack . A.encode

decodeElems :: (FromJSON (v (Fix v))) => String -> Maybe (Elems v)
decodeElems = A.decode . pack . S.encode

mockDbWriteH :: forall remEff val v.
  (Functor remEff, Lit Uuid <: v)
  => Handler_ (DbWrite (EntityDecl (Fix v))) val (Elems v) remEff (val, Elems v)
mockDbWriteH = Handler_
  { ret_ = curry pure
  , hdlr_ = \eff db@(Elems vars decls classes ) -> case eff of
    (SetVar (name, id) k) -> k
      $ Elems (insert (fromString name) id vars) decls classes
    (SetEntity e k) -> k
      $ updateEntity e db
  }

updateEntity e (Elems vars decls classes ) = Elems vars
  (insert (fromString $ getUuid e) e decls)
  (insertWith Set.union 
    (fromString $ projEName e) 
    (Set.singleton $ getUuid e) classes)

type MaybeEntity v = Maybe (EntityDecl (Fix v))
type TempEHeap v = MLState Address (MaybeEntity v)

tempEHeapH :: (Functor g) => Handler_ (TempEHeap v) val
  [(Address, (MaybeEntity v))] g (val, [(Address, MaybeEntity v)])
tempEHeapH = heap''

tempEHeapH' :: (Functor g) => Handler_ (TempEHeap v) val
  (Map Address (MaybeEntity v)) g val
tempEHeapH' = heap'