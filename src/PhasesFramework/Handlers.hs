module PhasesFramework.Handlers where
import Utils
import Actions.Effects
import Templates.Effects (TVarAddress)
import qualified Data.Map as Map
import Data.Maybe (fromJust)

cacheH :: Functor g => Handler_ (MLState TVarAddress (Fix v)) a
    (Map.Map TVarAddress (Fix v) ) g (a, [(TVarAddress, Fix v)])
cacheH = Handler_ 
    { ret_ = \v map -> pure (v, Map.toList map)
    , hdlr_ = \ eff map -> case eff of
        (Assign (name, value) k) -> k $ Map.insert name value map
        (Deref name k) -> k (fromJust $ Map.lookup name map) map
    }

cacheH' :: Functor g => Handler_ (MLState TVarAddress (Fix v)) a
    (Map.Map TVarAddress (Fix v) ) g a
cacheH' = Handler_ 
    { ret_ = \v map -> pure v
    , hdlr_ = \ eff map -> case eff of
        (Assign (name, value) k) -> k $ Map.insert name value map
        (Deref name k) -> k (fromJust $ Map.lookup name map) map
    }