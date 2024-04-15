module Eval.Handlers where
import Utils.Handler (Handler_ (..))
import Eval.Effects (MLState (..))
import qualified Data.Map as Map
import Syntax (Address)
import Data.Map (fromList)
import Eval.Syntax (VName)
import Utils.Denote
import Data.Maybe (mapMaybe)

newLoc :: Map.Map Address v -> Address
newLoc map =  Map.size map + 5

makeEnv :: [(Address, v)] -> Map.Map Address v
makeEnv = fromList

heap :: (Functor g) => Handler_ (MLState Address v) a (Map.Map Address v) g (a, [(Address, v)])
heap = Handler_
  { ret_ = \x map -> pure (x, Map.foldrWithKey (\key val list -> (key, val) : list) [] map )
  , hdlr_ = \x map -> case x of
      Ref value k -> let loc  = newLoc map
        in k loc $ Map.insert loc value map
      Deref key k -> k (case Map.lookup key map of Just x -> x) map
      Assign (key, value) k -> k $ Map.insert key value map }

heap' :: (Functor g) => Handler_ (MLState Address v) a (Map.Map Address v) g a
heap' = Handler_
  { ret_ = \x map -> pure x
  , hdlr_ = \x map -> case x of
      Ref value k -> let loc  = newLoc map
        in k loc $ Map.insert loc value map
      Deref key k -> k (case Map.lookup key map of Just x -> x) map
      Assign (key, value) k -> k $ Map.insert key value map }

environment :: (Functor g) => Handler_ (MLState VName Address) a (Env eff v) g (a, Env eff v)
environment = Handler_
  { ret_ = \x map -> pure (x, map)
  , hdlr_ = \x map -> case (varEnv map, x) of
      (env, Deref key k)     -> k (case lookup key env of Just x -> x) map
      (env, Assign record k) -> k $ insertMap record map }

insertMap :: (VName, Address) -> Env eff v  -> Env eff v
insertMap record env    = env { varEnv = record : varEnv env}
--insertMap record (EList envs) = EList $ map (insertMap record) envs

