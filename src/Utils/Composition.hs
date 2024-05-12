module Utils.Composition where
import Prelude hiding (sum)

--- FUNCTOR COMPOSITION

infixr 6 +
data (f + g) a
  = L (f a)
  | R (g a)
  deriving (Functor, Eq, Show)

infix 5 <:
class (Functor f, Functor g) => f <: g where
  inj  :: f k -> g k
  proj :: g k -> Maybe (f k)

instance {-# OVERLAPPING #-} (Functor f) => f <: f where
  inj :: f k -> f k
  inj = id
  proj :: f k -> Maybe (f k)
  proj = Just

instance (Functor f, Functor g) =>f  <: f + g where
  inj = L
  proj = \case
    L f -> Just f
    _ -> Nothing

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, f <: g', h ~ g + g') => f <: h where
  inj = R . inj 
  proj = \case
    L g -> Nothing
    R g' -> proj g'

--- VALUE COMPOSITION

infix 5 <
class u < v where
  injV  :: u -> v
  projV :: v -> Maybe u

infixr \/
type u \/ v = Either u v

instance {-# OVERLAPPING #-} u < u where
  injV :: u -> u
  injV = id
  projV :: u -> Maybe u
  projV = Just

instance u < u \/ v where
  injV :: u -> u \/ v
  injV = Left
  projV :: u \/ v -> Maybe u
  projV = \case
    Left f -> Just f
    _ -> Nothing

instance {-# OVERLAPPABLE #-} (u < v', w ~ v \/ v') => u < w where
  injV :: (u < v') => u -> v \/ v'
  injV = Right . injV 
  projV :: (u < v') => v \/ v' -> Maybe u
  projV = \case
    Left g -> Nothing
    Right g' -> projV g'

