module Utils.Composition where
import Prelude hiding (sum)

infixr 6 +
data (f + g) a
  = L (f a)
  | R (g a)
  deriving Functor

type f ->: g = forall a. f a -> g a

data f :<->: g = Iso { to :: f ->: g, from :: g ->: f }
{- which satisifies
     to . from = id
     from . to = id -}

sum :: (f a -> b) -> (g a -> b) -> (f + g) a -> b
sum f _ (L x) = f x
sum _ g (R x) = g x

isoRefl :: f :<->: f
isoRefl = Iso id id

isoSym :: f :<->: g -> g :<->: f
isoSym i = Iso (from i) (to i)

isoTrans :: f :<->: g -> g :<->: h -> f :<->: h
isoTrans i1 i2 = Iso (to i2 . to i1) (from i1 . from i2)

isoSumCong :: f :<->: f' -> g :<->: g' -> (f + g) :<->: (f' + g')
isoSumCong i1 i2 = Iso
  (sum (L . to i1) (R . to i2))
  (sum (L . from i1) (R . from i2))

isoSumComm :: (f + g) :<->: (g + f)
isoSumComm = Iso
  (sum R L)
  (sum R L)

isoSumAssoc :: (f + (g + h)) :<->: ((f + g) + h)
isoSumAssoc = Iso
  (sum (L . L) (sum (L . R) R))
  (sum (sum L (R . L)) (R . R))

data Forephism f g
  = forall f'. (Functor g, Functor f, Functor f') =>
      Forephism { iso :: g :<->: (f + f') }

infixr 4 <:
class (Functor f, Functor g) => f <: g where
  forephism :: Forephism f g

inj :: f <: g => f a -> g a
inj = case forephism of
    Forephism i -> from i . L

data End k -- No constructors!
  deriving Functor

instance Functor f => f <: f where
  forephism :: Functor f => Forephism f f
  forephism = Forephism (Iso
    L
    (sum id (\ (x :: End a) -> case x of)))

instance (Functor f, Functor g) => f <: f + g where
  forephism :: (Functor f, Functor g) => Forephism f (f + g)
  forephism = Forephism isoRefl

instance {-# OVERLAPPABLE #-} (Functor f, Functor g, Functor g', f <: g')
      => f <: g + g' where
  forephism :: (Functor f, Functor g, Functor g', f <: g') => Forephism f (g + g')
  forephism = case forephism of
    Forephism i -> Forephism
      (isoTrans
         (isoSumCong isoRefl i)
         (isoTrans isoSumComm (isoSym isoSumAssoc)))
