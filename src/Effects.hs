module Effects where
import Utils.Composition
import Utils.Free
import Bool.Syntax (LitB, OpB)
import Data.Kind
import Arith.Syntax (LitAr)

data Operation p k
  = OpBool p [LitB] (LitB -> k)
  | OpAr p [LitAr] (LitAr -> k)
  | OpAB p [LitAr] (LitB -> k)
 -- | forall v1 v2. (v1 < v, v2 < v) => OperHet p [v1] (v2 -> k) -- how bad of an idea is this?
  deriving Functor

type Constructor tIn tOut =  
    forall p k. p -> [tIn] -> (tOut -> k) 
    -> Operation p k

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

binaryOp' :: (Operation p <: f, v < a, v' < a) 
  => Constructor v v' -> p -> v -> v -> Free f a
binaryOp' c param e1 e2 = Op . inj
    $ c param [e1, e2] 
    $ Pure . injV

binaryOpHet :: (Operation p <: f, v < a, v' < a) => Constructor v v' -> p -> v -> v -> Free f a
binaryOpHet c param e1 e2 = Op . inj
    $ c param [e1, e2] 
    $ Pure . injV

class BinaryOperation p v f| p -> v where 
  op :: (Operation p <: f, v < v')  
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
