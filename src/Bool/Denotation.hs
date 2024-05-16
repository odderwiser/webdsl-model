module Bool.Denotation where
import Bool.Syntax
import Bool.Effects ( Cond, cond )
import Utils.Composition
import Utils.Free
import Utils.Fix
import Utils.Environment (FreeEnv)


op :: (Functor f, LitBool <: v') 
  => (Bool -> Bool -> Bool) -> Fix v' -> Fix v' -> Free f (Fix v')
op operand e1 e2 = case (projF e1, projF e2) of
  (Just (Lit e1'), Just (Lit e2')) -> return 
    $ injF $ Lit 
    $ operand e1' e2'

denote :: (Cond <: eff, LitBool <: v) 
  => Boolean (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (LitB bool) env = return $ injF $ Lit bool

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
