module Effects where
import Utils.Composition
import Utils.Free
import Utils.Denote (coerceFree, Literal (coerce))

data Operation p v k
  = Oper p [v] (v -> k)
  deriving Functor

binaryOp :: (Literal g, Operation p (g e)  <: f)
  => p
  -> g e
  ->  g e
  -> Free f (g e')
binaryOp param e1 e2 = coerceFree
  $ Op
  $ inj
  $ Oper param [e1, e2] Pure

data Cond v k = Cond v k k
  deriving Functor

cond :: (Literal a, Cond v <: f)
  => v
  -> Free f (a e)
  -> Free f (a e)
  -> Free f (a e')
cond x m1 m2 = coerceFree
  $ Op
  $ inj
  $ Cond x m1 m2
