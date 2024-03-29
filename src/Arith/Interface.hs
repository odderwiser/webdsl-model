module Arith.Interface (denote) where
import Arith.Syntax (Arith (..), OpArith (..))
import Utils.Denote (Env)
import Utils.Composition
import Utils.Free (Free (Op, Pure))
import Syntax (Type(Int))

op :: (Functor f, Int < v') 
  => (Int -> Int -> Int) -> v' -> v' 
  -> Free f v'
op operand e1 e2 = case (projV e1, projV e2) of
  (Just e1', Just e2') -> return 
    $ injV 
    $ operand e1' e2'


denote :: (Functor eff, Int < v) 
  => Arith (Env eff v -> Free eff v) 
  -> Env eff v-> Free eff v
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
