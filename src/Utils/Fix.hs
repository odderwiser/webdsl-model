{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils.Fix where
import Utils.Composition
import GHC.Generics (Generic)

data Fix f = In (f (Fix f))
    deriving Generic

data BiFix e f = BIn (e (BiFix e f) (Fix f))


deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Show (f (Fix f))) => Show (Fix f)

injBf :: (f <:: g) => f (BiFix g h) (Fix h)  -> BiFix g h
injBf = BIn . inj'

injF :: (f <: g) => f (Fix g) -> Fix g 
injF = In . inj

projF :: (f <: g) => Fix g -> Maybe (f (Fix g))
projF (In fix) = proj fix

cmapF :: (Functor g, g <<: h ) => Fix g -> Fix h
cmapF (In g) = In $ fmap cmapF (cmap g)

class (Functor g) => WeakenF h g where
    weakenF ::  Fix h -> Fix g

instance Functor c => WeakenF (a + b + c) c where
    weakenF (In fix) = case fix of
        (R (R c)) -> In $ fmap weakenF c