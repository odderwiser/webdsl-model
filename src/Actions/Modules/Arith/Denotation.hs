module Actions.Modules.Arith.Denotation (denote) where
import Actions.Modules.Arith.Syntax 
import Utils hiding (denote)
import Actions.Values

op :: (Functor f, Lit Int <: v') 
  => (Int -> Int -> Int) -> Maybe Int -> Maybe Int 
  -> Free f (Fix v')
op operand (Just e1) (Just e2) = v $ operand e1 e2

denote :: (Functor eff, LitInt <: v) 
  => Arith (FreeEnv eff (Fix v)) 
  -> FreeEnv eff (Fix v)
denote (LitAr int) env = v int

denote (OpArith Add a b) env = do 
  a' <- a env 
  b' <- b env 
  op (+) (projV a') (projV b')

denote (OpArith Div a b) env = do 
  a' <- a env 
  b' <- b env 
  op div (projV a') (projV b')

denote (OpArith Sub a b) env = do 
  a' <- a env 
  b' <- b env 
  op (-) (projV a') (projV b')

denote (OpArith Mul a b) env = do 
  a' <- a env 
  b' <- b env 
  op (*) (projV a') (projV b')

denote (OpArith Mod a b) env = do 
  a' <- a env 
  b' <- b env 
  op mod (projV a') (projV b')
