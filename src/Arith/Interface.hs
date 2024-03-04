module Arith.Interface (denote) where
import Effects (Operation (..), binaryOp', BinaryOperation (op))
import Arith.Syntax (Arith (..), OpArith, LitAr (Lit))
import Utils.Denote (Env)
import Utils.Composition
import Utils.Free (Free (Op, Pure))
import Prelude hiding (LT)

instance BinaryOperation OpArith LitAr f  where
  op :: (Operation OpArith <: f, LitAr < v') 
    => OpArith -> v' -> v' 
    -> Free f v'
  op param e1 e2 = case (projV e1 :: Maybe LitAr, projV e2 :: Maybe LitAr) of
    (Just e1', Just e2') -> binaryOp' OpAr param e1' e2'


denote :: (Operation OpArith <: eff, LitAr < v) 
  => Arith (Env -> Free eff v) 
  -> Env -> Free eff v
denote (LitAr int) env = return $ injV int

denote (OpArith ops a b) env = do 
  a' <- a env 
  b' <- b env 
  op ops a' b'
