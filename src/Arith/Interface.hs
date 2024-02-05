module Arith.Interface where
import Effects (Operation (Oper), binaryOp', BinaryOperation (op))
import Arith.Syntax (Arith (..), OpArith, LitAr (Lit))
import Utils.Denote
import Utils.Composition
import Utils.Free (Free (Op, Pure))
import Prelude hiding (LT)

instance BinaryOperation LitAr v' OpArith f  where
  op :: (Operation OpArith LitAr <: f, LitAr < v') => OpArith -> v' -> v' -> Free f v'
  op param e1 e2 = case (projV e1 :: Maybe LitAr, projV e2 :: Maybe LitAr) of
    (Just e1', Just e2') -> binaryOp' param e1' e2'


instance Functor eff => Denote 
    Arith (Operation OpArith LitAr + eff) LitAr 
    where
  denote :: Arith (Env -> Free (Operation OpArith LitAr + eff) LitAr) 
    -> Env -> Free (Operation OpArith LitAr + eff) LitAr
  denote (LitAr int) env = return int

  denote (OpArith ops a b) env = do 
    a' <- a env 
    b' <- b env 
    op ops a' b'
