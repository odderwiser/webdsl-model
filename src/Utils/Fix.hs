{-# LANGUAGE QuantifiedConstraints #-}
{-# LANGUAGE StandaloneDeriving #-}
{-# LANGUAGE UndecidableInstances #-}
module Utils.Fix where
import Utils.Composition
import GHC.Generics (Generic)

data Fix f = In (f (Fix f))
    deriving Generic

data BiFix e f = BIn (e (BiFix e f) f)

deriving instance (Eq (f (Fix f))) => Eq (Fix f)
deriving instance (Show (f (Fix f))) => Show (Fix f)

injBf :: (f <:: g) => f (BiFix g h) h  -> BiFix g h
injBf elem = BIn (inj' elem)

injF :: (f <: g) => f (Fix g) -> Fix g 
injF = In . inj

projF :: (f <: g) => Fix g -> Maybe (f (Fix g))
projF (In fix) = proj fix
