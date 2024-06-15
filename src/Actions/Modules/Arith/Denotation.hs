module Actions.Modules.Arith.Denotation (denote) where
import Actions.Modules.Arith.Syntax 
import Utils hiding (denote)
import Actions.Values

op :: (Functor f, LitInt <: v') 
  => (Int -> Int -> Int) -> Fix v' -> Fix v' 
  -> Free f (Fix v')
op operand e1 e2 = case (unbox' e1, unbox' e2) of
  (e1', e2') -> return 
    $ boxV 
    $ operand e1' e2'

denote :: (Functor eff, LitInt <: v) 
  => Arith (FreeEnv eff (Fix v)) 
  -> FreeEnv eff (Fix v)
denote (LitAr int) env = return $ boxV int

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
