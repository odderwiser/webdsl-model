module Bool.Effects where
import Utils.Composition 
import Utils.Free (Free(..))

data Cond k =  Cond Bool k k
  deriving Functor

cond :: (Cond <: f, Bool < a) 
  => a -> Free f a -> Free f a 
  -> Free f a
cond bool k1 k2 = case projV bool :: Maybe Bool of
  Just bool' -> Op . inj 
    $ Cond (injV bool') k1 k2
