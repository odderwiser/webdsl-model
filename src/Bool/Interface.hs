module Bool.Interface (denote) where
import Utils.Denote (Env)
import Bool.Syntax
import Effects
    ( Cond, Operation(OpBool), binaryOp', cond )
import Utils.Composition
import Utils.Free


op :: (Functor f, LitB < v') 
  => (Bool -> Bool -> Bool) -> v' -> v' -> Free f v'
op operand e1 e2 = case (projV e1, projV e2) of
  (Just (Lit e1'), Just (Lit e2')) -> return 
    $ injV 
    $ Lit 
    $ operand e1' e2'

denote :: (Cond <: eff, LitB < v) 
  => Boolean (Env -> Free eff v)
  -> Env -> Free eff v
denote (LitB bool) env = return $ injV bool

denote (OpB Or a b) env = do
  a' <- a env
  b' <- b env
  op (||) a' b'

denote (OpB And a b) env = do
  a' <- a env
  b' <- b env
  op (&&) a' b'

denote (If c a b) env = do
  c' <- c env
  cond c' (a env) (b env)
