module Col.Composition where
import Col.Syntax
import Utils.Composition 
import Data.Maybe (mapMaybe)

instance {-# OVERLAPPABLE #-}(v < u, w~ Recursive u) => v < Recursive u where
  injV :: (v < u) => v -> Recursive u
  injV = Base . injV
  projV :: (v < u) => Recursive u -> Maybe v
  projV (Base v) = projV v
  projV (Rec v) = Nothing 

instance {-# OVERLAPPING #-} (v < u, w~ Recursive u) => [v] < Recursive u where
  injV :: (v < u) => [v] -> Recursive u
  injV = Rec . map injV
  projV :: (v < u) => Recursive u -> Maybe [v]
  projV (Base v) = Nothing
  projV (Rec v) = Just $ mapMaybe projV v

-- manual inference of generic lists
projList :: Eq v => Recursive v -> Maybe [Recursive v]
projList (Base v) = Nothing
projList (Rec v) = Just v