module Effects where
import Utils.Composition
import Utils.Free
import Bool.Syntax (LitB)

data Operation p v k
  = Oper p [v] (v -> k)
  deriving Functor

-- binaryOp :: (Operation p v <: f, v < v') => p -> v' -> v' -> Free f v
-- binaryOp param e1 e2 = case (projV e1 :: (forall v. v < v' => Maybe v), projV e2 :: Maybe v) of
--   (Just e1', Just e2') -> Op . inj 
--     $ Oper param [e1, e2] 
--     $ Pure 

binaryOp :: (Operation p v <: f, v < v') => p -> v -> v -> Free f v'
binaryOp param e1 e2 = Op . inj 
    $ Oper param [e1, e2] 
    $ Pure . injV

data Cond k =  Cond LitB k k
  deriving Functor

cond :: (Cond <: f, LitB < a) => a -> Free f a -> Free f a -> Free f a
cond bool k1 k2 = case projV bool :: Maybe LitB of
  Just bool' -> Op . inj 
    $ Cond (injV bool') k1 k2
  
-- cond :: (Cond <: f, LitB < a) => LitB -> Free f a -> Free f a -> Free f a
-- cond bool k1 k2 =
--   Op 
--   $ inj 
--   $ Cond bool k1 k2
