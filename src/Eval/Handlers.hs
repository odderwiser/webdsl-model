module Eval.Handlers where
import Utils.Handler (Handler_ (..))
import Eval.Effects (MLState (..))
import qualified Data.Map as Map
import Syntax (Address)
import Data.Map (fromList)
import Eval.Syntax (VName)
import Data.Maybe (mapMaybe, fromJust)
import Utils.Environment

newLoc :: Map.Map Address v -> Address
newLoc = Map.size

makeEnv :: [(Address, v)] -> Map.Map Address v
makeEnv = fromList

heap :: (Functor g) => Handler_ (MLState Address v) a (Map.Map Address v) g (a, [(Address, v)])
heap = heapHandler (\output store ->
    (output,
    Map.foldrWithKey (\key val list -> (key, val) : list) [] store))
  Map.size
  Map.lookup
  Map.insert
  -- Handler_
  -- { ret_ = \x map -> pure (x, Map.foldrWithKey (\key val list -> (key, val) : list) [] map )
  -- , hdlr_ = \x map -> case x of
  --     Ref value k -> let loc  = newLoc map
  --       in k loc $ Map.insert loc value map
  --     Deref key k -> k (fromJust $ Map.lookup key map) map
  --     Assign (key, value) k -> k $ Map.insert key value map }

heap' :: (Functor g) => Handler_ (MLState Address v) a (Map.Map Address v) g a
heap' = heapHandler const Map.size Map.lookup Map.insert

  -- Handler_
  -- { ret_ = \x map -> pure x
  -- , hdlr_ = \x map -> case x of
  --     Ref value k -> let loc  = newLoc map
  --       in k loc $ Map.insert loc value map
  --     Deref key k -> k (fromJust $ Map.lookup key map) map
  --     Assign (key, value) k -> k $ Map.insert key value map }

heap'' :: (Functor g) => 
  Handler_ (MLState Address v) a [(Address, v)] g (a, [(Address, v)])
heap'' = heapHandler (,) length lookup (\k v list -> (k, v) : list)

-- heapHandler :: Functor remEff
--   => (t1 -> col -> output)
--   -> (Address -> col -> Maybe v)
--   -> (Address -> t2 -> col -> col)
--   -> Handler_ (MLState Address v) t1 col remEff output
heapHandler ret' size' lookup' insert' = Handler_ {
  ret_ = \output store -> pure $ ret' output store
  ,hdlr_ = \effect store -> case effect of
    Ref value k -> let loc  = size' store
        in k loc $ insert' loc value store
    Deref key k -> k (fromJust $ lookup' key store) store
    Assign (key, value) k -> k $ insert' key value store }

environment :: (Functor g) => Handler_ (MLState VName Address)
  a (Env eff v) g (a, Env eff v)
environment = mkAHandler varEnv lookup insertMap

insertMap :: (VName, Address) -> Env eff v  -> Env eff v
insertMap record env    = env { varEnv = record : varEnv env}
--insertMap record (EList envs) = EList $ map (insertMap record) envs

