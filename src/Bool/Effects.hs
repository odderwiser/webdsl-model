module Bool.Effects where
import Utils.Composition 
import Utils.Free (Free(..))
import Bool.Syntax
import Utils.Fix

data Cond k =  Cond Bool k k
  deriving Functor

cond :: (fix ~ Fix a, Cond <: f, LitBool <: a) 
  => fix -> Free f fix -> Free f fix 
  -> Free f fix
cond bool k1 k2 = case projF bool of
  Just (Lit bool') -> Op . inj
    $ Cond bool' k1 k2
