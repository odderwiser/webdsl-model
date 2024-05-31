module Actions.Modules.Arith.Denotation (denote) where
import Actions.Modules.Arith.Syntax 
import Utils hiding (denote)

op :: (Functor f, LitInt <: v') 
  => (Int -> Int -> Int) -> Fix v' -> Fix v' 
  -> Free f (Fix v')
op operand e1 e2 = case (projF e1, projF e2) of
  (Just (Lit e1'), Just (Lit e2')) -> return 
    $ injF $ Lit 
    $ operand e1' e2'

denote :: (Functor eff, LitInt <: v) 
  => Arith (FreeEnv eff (Fix v)) 
  -> FreeEnv eff (Fix v)
denote (LitAr int) env = return $ injF (Lit int)

denote (OpArith Add a b) env = do 
  a' <- a env 
  b' <- b env 
  op (+) a' b'

denote (OpArith Div a b) env = do 
  a' <- a env 
  b' <- b env 
  op div a' b'

denote (OpArith Sub a b) env = do 
  a' <- a env 
  b' <- b env 
  op (-) a' b'

denote (OpArith Mul a b) env = do 
  a' <- a env 
  b' <- b env 
  op (*) a' b'

denote (OpArith Mod a b) env = do 
  a' <- a env 
  b' <- b env 
  op mod a' b'
