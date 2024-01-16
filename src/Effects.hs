module Effects where
import Free

data Operation p v k
  = Oper p [v] (v -> k)
  deriving Functor

binaryOp :: Operation p v <: f => p -> v -> v -> Free f v
binaryOp param e1 e2 = Op $ inj $ Oper param [e1, e2] Pure