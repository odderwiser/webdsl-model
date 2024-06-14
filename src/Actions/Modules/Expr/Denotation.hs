module Actions.Modules.Expr.Denotation where
import Actions.Modules.Expr.Syntax
import Actions.Modules.Bool.Syntax as B
import Actions.Arith
import Utils
import Syntax (Type (..))
import Foreign (fromBool)
import Actions.Values

op :: (Functor f, LitBool <: v)
  => (a -> a -> Bool) -> a -> a -> Free f (Fix v)
op operand e1 e2 = return
  $ box
  $ operand e1 e2

opCmp :: (fix ~ Fix v, Functor f, Num a, LitInt <: v, LitBool <: v)
  => (a -> a -> Bool) -> fix -> fix
  -> Free f fix
opCmp operand exp1 exp2 = op operand (toNum exp1) (toNum exp2)  

toNum :: forall a g. (Num a, LitInt <: g) => Fix g -> a
toNum = fromIntegral . (unbox :: Fix g -> Int)
    
    

denote :: (fix ~ Fix v,  Eq (v (Fix v)), 
  Functor eff, LitInt <: v, LitBool <: v)
  => Expr (Env eff fix -> Free eff fix)
  -> Env eff fix -> Free eff fix

denote (OpCmp Eq e1 e2) env = do
  e1' <- e1 env
  e2' <- e2 env
  op (==) e1' e2'

denote (OpCmp Neq e1 e2) env = do
  e1' <- e1 env
  e2' <- e2 env
  op (/=) e1' e2'

denote (OpCmp Lt e1 e2) env = do
  e1' <- e1 env
  e2' <- e2 env
  opCmp (<) e1' e2'

denote (OpCmp Lte e1 e2) env = do
  e1' <- e1 env
  e2' <- e2 env
  opCmp (<=) e1' e2'

denote (OpCmp Gt e1 e2) env = do
  e1' <- e1 env
  e2' <- e2 env
  opCmp (>) e1' e2'

denote (OpCmp Gte e1 e2) env = do
  e1' <- e1 env
  e2' <- e2 env
  opCmp (>=) e1' e2'
