module Free where

import Prelude hiding (read, Read, sum)

data Free f a
  = Pure a
  | Op (f (Free f a))

instance Functor f => Functor (Free f) where
  fmap :: Functor f => (a -> b) -> Free f a -> Free f b
  fmap f = fold (pure . f) Op

instance Functor f => Applicative (Free f) where
  pure :: Functor f => a -> Free f a
  pure = Pure
  (<*>) :: Functor f => Free f (a -> b) -> Free f a -> Free f b
  f <*> m = fold (`fmap` m) Op f

instance Functor f => Monad (Free f) where
  (>>=) :: Functor f => Free f a -> (a -> Free f b) -> Free f b
  (>>=) m k = fold k Op m

fold :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
fold gen _   (Pure x) = gen x
fold gen alg (Op f)   = alg (fmap (fold gen alg) f)

infixr 6 +
data (f + g) a
  = L (f a)
  | R (g a)
  deriving Functor

type f ->: g = forall a. f a -> g a

permute :: (Functor f, Functor f')
        => (f ->: f') -> Free f a -> Free f' a
permute f = fold Pure (Op . f)

data Handler f a f' b
  = Handler { ret  :: a -> Free f' b
            , hdlr :: f (Free f' b) -> Free f' b }

handle :: (Functor f, Functor f')
       => Handler f a f' b -> Free (f + f') a -> Free f' b
handle h = fold
  (ret h)
  (\x -> case x of
     L y -> hdlr h y
     R y -> Op y)
     

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

data End k -- No constructors!
  deriving Functor


inj :: f <: g => f a -> g a
inj = case forephism of
  Forephism i -> from i . L

instance Functor f => f <: f where
  forephism :: Functor f => Forephism f f
  forephism = Forephism (Iso
    L
    (sum id (\(x :: End a) -> case x of)))

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

mask :: Functor f => Free f a -> Free (f' + f) a
mask = fold Pure (Op . R)

hup :: f <: g => (forall f'. Functor f' => Free (f + f') a -> Free f' b)
    -> Free g a -> Free g b
hup h = case forephism of
  Forephism i -> permute (from i) . mask . h . permute (to i)

unwrap :: Free End a -> a
unwrap (Pure x) = x
unwrap (Op f) = case f of

data Handler_ handledEff val param remEff output
  = Handler_ { ret_  :: val -> (param -> Free remEff output)
             , hdlr_ :: handledEff (param -> Free remEff output) 
             -> (param -> Free remEff output) }

handle_ :: (Functor handledEff, Functor remEff)
        => Handler_ handledEff val param remEff output 
        -> Free (handledEff + remEff) val -> param -> Free remEff output

handle_ handler = fold
  (ret_ handler)
  (\effect -> case effect of
     L handledEff -> hdlr_ handler handledEff
     R remainder -> \param -> Op (fmap (\apply -> apply param) remainder))

flipHandle_ :: (a -> b -> c -> d) -> a -> c -> b -> d 
flipHandle_ fun a c b = fun a b c 
