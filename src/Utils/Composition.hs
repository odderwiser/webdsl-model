{-# LANGUAGE QuantifiedConstraints #-}
module Utils.Composition where
import Prelude hiding (sum)
import Data.Bifunctor (Bifunctor (bimap))

--- FUNCTOR COMPOSITION

data End k
  deriving (Functor)

instance Show (End k) where
  show :: End k -> String
  show x = "x" 

infixr 6 +
data (f + g) a
  = L (f a)
  | R (g a)
  deriving (Functor, Eq)

instance (Show (f a), Show (g a)) => Show ((f + g) a) where
  show :: (Show (f a), Show (g a)) => (+) f g a -> String
  show (L f) = show f
  show (R g) = show g

infix 5 <:
class (Functor f, Functor g) => f <: g where
  inj  :: f k -> g k
  proj :: g k -> Maybe (f k)
  -- cmap :: (forall f. f <: g, forall i. i <: h) => g k -> h k

-- cmap :: (forall f. f <: g, f <: h) => g k -> g k

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

infixr 6 +:
data (f +: g) a b 
  = L' (f a b)
  | R' (g a b)

instance (Bifunctor f, Bifunctor g) => Bifunctor (f +: g) where
  bimap :: (a -> b) -> (c -> d) -> (+:) f g a c -> (+:) f g b d
  bimap f g (L' a) = L' $ bimap f g a
  bimap f g (R' b) = R' $ bimap f g b

infix 5 <::
class f <:: g where
  inj'  :: f a k -> g a k
  proj' :: g a k -> Maybe (f a k)

instance  f  <:: f  where
  inj' = id
  proj' =  Just

instance f  <:: f +: g where
  inj' = L'
  proj' = \case
    L' f -> Just f
    _ -> Nothing

instance {-# OVERLAPPABLE #-}  (f <:: g', h ~ g +: g') 
  => f <:: h where
  inj' = R' . inj' 
  proj' = \case
    L' g -> Nothing
    R' g' -> proj' g'

-- natural transformation from type to type
infix <<:
class (Functor g, Functor h) => g <<: h where
  cmap :: g k -> h k

instance {-# OVERLAPPING #-}  (Functor f, Functor h,
   f <: h, f' <: h) => (f + f') <<: h where
  cmap = \case
    L f -> inj f
    R f -> inj f

instance {-# OVERLAPPABLE #-} (Functor f, Functor g', g <<: h,
  f <: h, (g + g')<<: h) => (f + (g + g')) <<: h where
  cmap = \case
    L f -> inj f
    R f -> cmap f


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

