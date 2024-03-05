module Expr.Interface where
import Effects (Operation (OpBool, OpAB), binaryOp', binaryOpHet, BinaryOperation (op))
import Expr.Syntax
import Bool.Syntax (LitB, OpB)
import Utils.Composition (type (<:), type (<), projV)
import Utils.Free (Free)
import Arith.Syntax (LitAr)

class CmpOperation v1 where 
  opC :: (Operation Cmp <: f, v1 < v', LitB < v')  
    => Cmp -> v' -> v' 
    -> Free f v'

instance CmpOperation LitB where
  opC :: (Operation Cmp <: f, LitB < v') => Cmp -> v' -> v' -> Free f v'
  opC param e1 e2 = case (projV e1 :: Maybe LitB, projV e2 :: Maybe LitB) of
    (Just e1', Just e2') -> binaryOp' OpBool param e1' e2'

instance CmpOperation LitAr   where
  opC :: (Operation Cmp <: f, LitAr < v', LitB < v') => Cmp -> v' -> v' -> Free f v'
  opC param e1 e2 = case (projV e1 :: Maybe LitAr, projV e2 :: Maybe LitAr) of
    (Just e1', Just e2') -> binaryOpHet OpAB param e1' e2'