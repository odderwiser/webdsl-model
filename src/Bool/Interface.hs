module Bool.Interface where
import Utils.Denote
import Bool.Syntax
import Effects
import Utils.Composition
import Utils.Free

instance BinaryOperation LitB v' OpB f  where
  op :: (Operation OpB LitB <: f, LitB < v') => OpB -> v' -> v' -> Free f v'
  op param e1 e2 = case (projV e1 :: Maybe LitB, projV e2 :: Maybe LitB) of
    (Just e1', Just e2') -> binaryOp' param e1' e2'

instance forall eff v.
  (Cond <: eff, Operation OpB LitB <: eff, LitB < v)
  => Denote Boolean eff v where
  denote :: (Cond <: eff, Operation OpB LitB <: eff, LitB < v) 
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
