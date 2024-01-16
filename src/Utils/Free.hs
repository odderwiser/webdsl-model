module Utils.Free where

data Free f a
  = Pure a
  | Op (f (Free f a))

fold :: Functor f => (a -> b) -> (f b -> b) -> Free f a -> b
fold gen _   (Pure x) = gen x
fold gen alg (Op f)   = alg (fmap (fold gen alg) f)

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
