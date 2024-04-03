module Bool.Interface (denote) where
import Utils.Denote (Env, FreeEnv)
import Bool.Syntax
import Bool.Effects ( Cond, cond )
import Utils.Composition
import Utils.Free


op :: (Functor f, Bool < v') 
  => (Bool -> Bool -> Bool) -> v' -> v' -> Free f v'
op operand e1 e2 = case (projV e1, projV e2) of
  (Just e1', Just e2') -> return 
    $ injV 
    $ operand e1' e2'

denote :: (Cond <: eff, Bool < v) 
  => Boolean (FreeEnv eff v)
  -> FreeEnv eff v
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
