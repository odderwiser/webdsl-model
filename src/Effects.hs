module Effects where
import Utils.Composition
import Utils.Free

data Operation p v k
  = Oper p [v] (v -> k)
  deriving Functor

binaryOp :: Operation p v <: f => p -> v -> v -> Free f v
binaryOp param e1 e2 = Op $ inj $ Oper param [e1, e2] Pure

data Cond v k =  Cond v k k
  deriving Functor

cond :: Cond v <: f => v -> Free f a -> Free f a -> Free f a
cond x m1 m2 = Op $ inj $ Cond x m1 m2
