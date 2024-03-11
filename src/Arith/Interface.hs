module Arith.Interface (denote) where
import Arith.Syntax (Arith (..), OpArith (..), LitAr (Lit))
import Utils.Denote (Env)
import Utils.Composition
import Utils.Free (Free (Op, Pure))
import Syntax (Type(Int))

op :: (Functor f, LitAr < v') 
  => (Int -> Int -> Int) -> v' -> v' 
  -> Free f v'
op operand e1 e2 = case (projV e1, projV e2) of
  (Just (Lit e1'), Just (Lit e2')) -> return 
    $ injV 
    $ Lit 
    $ operand e1' e2'


denote :: (Functor eff, LitAr < v) 
  => Arith (Env -> Free eff v) 
  -> Env -> Free eff v
denote (LitAr int) env = return $ injV int

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
