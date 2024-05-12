{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils.Fix where
import Utils.Composition

data Fix f = In (f (Fix f))

-- instance Eq (Fix f) where
--   (==) :: Fix f -> Fix f -> Bool
--   (==) (In a) (In b) = a == b
--the benefits of using a recursive datatype
--would be that equality by construction is possible

-- instance (forall a. Eq a => Eq (f a)) => Eq (Fix f) where
--   (==) :: (forall a. Eq a => Eq (f a)) => Fix f -> Fix f -> Bool
--   In f == In g = f == g

deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Show (f (Fix f))) => Show (Fix f)

injF :: (f <: g) => f (Fix g) -> Fix g 
injF = In . inj

projF :: (f <: g) => Fix g -> Maybe (f (Fix g))
projF (In fix) = proj fix

class (f <: g) => BinaryInject f g op where
    bin ::  op -> g (Fix g) -> g (Fix g) -> f (Fix g)
