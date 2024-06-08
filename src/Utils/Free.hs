module Utils.Free where
import Utils.Composition

data Free f a
  = Pure a
  | Op (f (Free f a))

instance (Show a, Show (f (Free f a))) => Show (Free f a) where
  show :: Free f a -> String
  show (Pure a ) = "(P " ++ show a ++ ")"
  show (Op f ) = show f

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

projFree :: (f <: g) => Free g a -> Maybe (f (Free g a))
projFree (Pure a) = Nothing
projFree (Op g) = proj g  

injFree :: (f <: g) => f (Free g a) -> Free g a 
injFree = Op . inj

instance (Eq a, Eq (f (Free f a))) => Eq (Free f a) where
  (==) :: Eq a => Free f a -> Free f a -> Bool
  (==) (Pure a) (Pure b) = a == b 
  (==) (Op a) (Op b) = a == b
  (==) _ _ = False

