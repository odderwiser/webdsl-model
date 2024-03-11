module Bool.Effects where
import Bool.Syntax (LitB)
import Utils.Composition 
import Utils.Free (Free(..))

data Cond k =  Cond LitB k k
  deriving Functor

cond :: (Cond <: f, LitB < a) 
  => a -> Free f a -> Free f a 
  -> Free f a
cond bool k1 k2 = case projV bool :: Maybe LitB of
  Just bool' -> Op . inj 
    $ Cond (injV bool') k1 k2
