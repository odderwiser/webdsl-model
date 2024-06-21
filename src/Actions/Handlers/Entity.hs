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
import qualified Data.Aeson.KeyMap as KM (KeyMap, insert, union, insertWith, unionWith, lookup, empty, elems)
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

--this is the database
data Elems v = Elems
  { vars     :: KM.KeyMap Uuid
  , entities :: KM.KeyMap (EntityDecl (Fix v))
  , classes  :: KM.KeyMap (Set.Set Uuid)
  }
  deriving Generic

data WriteOps v = WriteGV VName Uuid | UpdateProperties Uuid (Map.Map PName (Fix v)) | AddEntity Uuid (EntityDecl (Fix v)) | AddClassMember EName Uuid

instance (FromJSON (v (Fix v))) => FromJSON (Elems v)
instance (ToJSON (v (Fix v))) => ToJSON (Elems v)
instance (ToJSON v) => ToJSON (EntityDecl v)
instance (FromJSON v) => FromJSON (EntityDecl v)
instance (ToJSON (v (Fix v))) => ToJSON (Fix v)
instance (FromJSON (v (Fix v))) => FromJSON (Fix v)

dbWriteH :: forall remEff val v.
  (Functor remEff, Lit Uuid <: v,ToJSON (v (Fix v)), FromJSON (v (Fix v)))
  => FilePath -> Handler_ (DbWrite (EntityDecl (Fix v)) (Fix v)) val [WriteOps v] remEff (IO val)
dbWriteH dbEntry = Handler_
  { ret_ = \ val writeOps -> pure $ do
    fileExists <- doesFileExist dbEntry
    oldDbState <- if fileExists then readFile' dbEntry else return ""
    case oldDbState of
      "" -> writeFile dbEntry $ encodeElems $ makeDb (Elems {vars = KM.empty, classes = KM.empty, entities = KM.empty}) writeOps
      _  -> case decodeElems oldDbState of
        Nothing -> writeFile dbEntry $ encodeElems $ makeDb (Elems {vars = KM.empty, classes = KM.empty, entities = KM.empty}) writeOps
        (Just (elems :: Elems v)) -> do
          writeFile dbEntry
            $ encodeElems
            $ updateDatabase elems writeOps
    return val
  , hdlr_ = \eff writeOps -> case eff of
    (SetVar (name, id) k) -> k
      $  writeOps ++ [WriteGV name id] --Elems (insert (fromString name) id vars) decls classes
    (SetEntity e k) ->
      let uuid = getUuid e in
      k $ writeOps ++ [AddClassMember (projEName e) uuid, AddEntity uuid e]  --updateEntity e db
    (UpdateEntity uuid pName v k) -> k $ updateWriteOps writeOps (UpdateProperties uuid $ Map.singleton pName v)
  }

updateWriteOps (AddEntity uuid e : tail) v@(UpdateProperties uuid' props)
  | uuid == uuid' = AddEntity uuid (updateProps e props) : tail
  | otherwise     = AddEntity uuid e : updateWriteOps tail v
updateWriteOps [] v = [v]
updateWriteOps (UpdateProperties uuid props : tail) v@(UpdateProperties uuid' props')
  | uuid == uuid' = UpdateProperties uuid (Map.union props' props) : tail
  | otherwise     = UpdateProperties uuid props : updateWriteOps tail v
updateWriteOps (somethingElse : tail) v@(UpdateProperties uuid' props') = somethingElse : updateWriteOps tail v

makeDb :: (Functor v) => Elems v -> [WriteOps v] -> Elems v
makeDb = makeDatabase makeDb

-- makeDatabase :: Elems v -> [WriteOps v] -> Elems v
makeDatabase :: (Functor v) => (Elems v -> [WriteOps v] -> Elems v) ->  Elems v -> [WriteOps v] -> Elems v
makeDatabase f elems (WriteGV name id : tail) = f elems { vars = KM.insert (fromString name) id (vars elems)} tail
makeDatabase f elems (AddEntity name e : tail) = f elems {entities = KM.insert (fromString name) e (entities elems) } tail
makeDatabase f elems (AddClassMember name id : tail) = f elems {classes = KM.insertWith Set.union (fromString name) (Set.singleton id) (classes elems) } tail
makeDatabase f elems (UpdateProperties name props : tail) = let entity = fromJust $ KM.lookup (fromString name) (entities elems)
  in f elems { entities = KM.insert (fromString name) (updateProps entity props) (entities elems)} tail
makeDatabase f elems [] = elems

updateDatabase :: (Functor v) => Elems v -> [WriteOps v] -> Elems v
updateDatabase elems (UpdateProperties name props : tail) = let entity = fromJust $ KM.lookup (fromString name) (entities elems)
  in updateDatabase elems { entities = KM.insert (fromString name) (updateProps entity props) (entities elems)} tail
updateDatabase elems ops = makeDatabase updateDatabase elems ops 

updateProps :: EntityDecl (Fix v) -> Map.Map PName (Fix v) -> EntityDecl (Fix v)
updateProps (EDecl name props) props' = EDecl name $ Map.toList $ Map.union props' $ Map.fromList props

update :: KM.KeyMap a -> KM.KeyMap a -> KM.KeyMap a
update oldMap newMap = KM.union newMap oldMap

encodeElems :: (ToJSON (v (Fix v))) => Elems v -> String
encodeElems = S.decode . unpack . A.encode

decodeElems :: (FromJSON (v (Fix v))) => String -> Maybe (Elems v)
decodeElems = A.decode . pack . S.encode

mockDbWriteH :: forall remEff val v.
  (Functor remEff, Lit Uuid <: v)
  => Handler_ (DbWrite (EntityDecl (Fix v)) (Fix v)) val (Elems v) remEff (val, Elems v)
mockDbWriteH = Handler_
  { ret_ = curry pure
  , hdlr_ = \eff db@(Elems vars decls classes ) -> case eff of
    (SetVar (name, id) k) -> k
      $ Elems (KM.insert (fromString name) id vars) decls classes
    (SetEntity e k) -> k
      $ updateEntity e db
  }

updateEntity e (Elems vars decls classes ) = Elems vars
  (KM.insert (fromString $ getUuid e) e decls)
  (KM.insertWith Set.union
    (fromString $ projEName e)
    (Set.singleton $ getUuid e) classes)

type MaybeEntity v = Maybe (EntityDecl (Fix v))
type TempEHeap v = MLState Address (MaybeEntity v)

tempEHeapH :: (Functor g) => Handler_ (TempEHeap v) val
  [(Address, (MaybeEntity v))] g (val, [(Address, MaybeEntity v)])
tempEHeapH = heap''

tempEHeapH' :: (Functor g) => Handler_ (TempEHeap v) val
  (Map.Map Address (MaybeEntity v)) g val
tempEHeapH' = heap'