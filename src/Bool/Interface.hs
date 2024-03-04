module Bool.Interface (denote) where
import Utils.Denote (Env)
import Bool.Syntax
import Effects
import Utils.Composition
import Utils.Free

instance BinaryOperation OpB LitB f  where
  op :: (Operation OpB  <: f, LitB < v') => OpB -> v' -> v' -> Free f v'
  op param e1 e2 = case (projV e1 :: Maybe LitB, projV e2 :: Maybe LitB) of
    (Just e1', Just e2') -> binaryOp' OpBool param e1' e2'

denote :: (Cond <: eff, Operation OpB <: eff, LitB < v) 
  => Boolean (Env -> Free eff v)
  -> Env -> Free eff v
denote (LitB bool) env = return $ injV bool

denote (OpB ops a b) env = do
  a' <- a env
  b' <- b env
  op ops a' b'

denote (If c a b) env = do
  c' <- c env
  cond c' (a env) (b env)
