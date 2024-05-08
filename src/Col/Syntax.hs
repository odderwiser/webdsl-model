module Col.Syntax where
import Syntax (Type)
import Utils.Composition
import Data.Maybe (mapMaybe)

-- data Eq v => Recursive v = Rec [Recursive v] | Base v
--     deriving (Eq, Show)

-- instance {-# OVERLAPPABLE #-} (v < u, w ~ Recursive u, z ~ Recursive v) => Recursive v < Recursive u where
--   injV :: Recursive v -> Recursive u
--   injV (Base v) = Base $ injV v
--   injV (Rec v) = Rec $ map injV v
--   projV :: Recursive u -> Maybe (Recursive v)
--   projV (Base v) = case (projV v :: Maybe v) of
--     Just v' -> Just $ Base v' 
--     Nothing -> Nothing
--   projV (Rec v) = Just $ Rec $ mapMaybe projV v

-- data Col e 
--   = LitC [e]  
--   | In Type e e
--   -- | Add e e this is perhaps a statement
--     deriving Functor