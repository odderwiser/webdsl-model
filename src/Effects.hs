module Effects where
import Utils.Composition
import Utils.Free
import Bool.Syntax (LitB, OpB)
import Data.Kind

data Operation p v k
  = Oper p [v] (v -> k)
 -- | forall v1 v2. (v1 < v, v2 < v) => OperHet p [v1] (v2 -> k) -- how bad of an idea is this?
  deriving Functor

-- binaryOp :: forall v v' p f. (Operation p v <: f, v < v')  
--   => p -> v' -> v' 
--   -> Free f v'
-- binaryOp param e1 e2 = case (projV e1 :: Maybe v, projV e2 :: Maybe v) of
--   (Just e1', Just e2') -> Op . inj
--     $ Oper param [e1', e2'] 
--     $ Pure . injV

-- why is this necessary to do it this way?
-- the old code could not infer what v should be,
-- and I couldn't figure out how to inject the correct constraint as type hint 

binaryOp' :: (Operation p v <: f, v < a) => p -> v -> v -> Free f a
binaryOp' param e1 e2 = Op . inj
    $ Oper param [e1, e2] 
    $ Pure . injV

class BinaryOperation v v' p f | v -> p, p -> v where
  op :: (Operation p v <: f, v < v')  
    => p -> v' -> v' 
    -> Free f v'

data Cond k =  Cond LitB k k
  deriving Functor

cond :: (Cond <: f, LitB < a) 
  => a -> Free f a -> Free f a 
  -> Free f a
cond bool k1 k2 = case projV bool :: Maybe LitB of
  Just bool' -> Op . inj 
    $ Cond (injV bool') k1 k2
