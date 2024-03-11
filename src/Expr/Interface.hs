module Expr.Interface where
import Expr.Syntax
import Bool.Syntax as B
import Utils.Composition (type (<:), type (<) (injV), projV)
import Utils.Free (Free)
import Arith.Syntax (LitAr)
import Utils.Denote (Env)
import qualified Arith.Interface as A
import qualified Bool.Interface as B
import Syntax (Type (..))
import qualified Arith.Syntax as A
import Foreign (fromBool)

op :: (Functor f, Num a, LitB < v)
  => (a -> a -> Bool) -> a -> a -> Free f v
op operand e1 e2 = return 
  $ injV 
  $ B.Lit 
  $ operand e1 e2

opAr :: (Functor f, Num a, LitAr < v, LitB < v) 
  => (a -> a -> Bool) -> v -> v -> Free f v
opAr operand exp1 exp2 = 
  case (projV exp1, projV exp2) of
  (Just (A.Lit e1'), Just (A.Lit e2')) -> 
    op operand (fromIntegral e1') (fromIntegral e2')

opB :: (Functor f, Num a, LitB < v) 
  => (a -> a -> Bool) -> v -> v -> Free f v
opB operand exp1 exp2 = 
  case (projV exp1, projV exp2) of
    (Just (B.Lit e1'), Just (B.Lit e2')) -> 
      op operand (fromBool e1') (fromBool e2')

opEq :: (Functor f, Num a, LitB < v, LitAr < v)
  => (a -> a -> Bool) -> (v, Type) -> (v, Type) 
  -> Free f v
opEq operand (exp1, type1) (exp2, type2) = 
  case (type1, type2) of
  (Int,  Int)  -> opAr operand exp1 exp2
  (Bool, Bool) -> opB  operand exp1 exp2
      
opCmp :: (Functor f, Num a, LitB < v, LitAr < v)
  => (a -> a -> Bool) -> (v, Type) -> (v, Type) 
  -> Free f v
opCmp operand (exp1, _) (exp2, _) = opAr operand exp1 exp2

denote :: (Functor eff, LitAr < v, LitB < v)
  => Expr (Env -> Free eff v)
  -> Env -> Free eff v

denote (OpCmp Eq (e1, t1) (e2, t2)) env = do
  e1' <- e1 env 
  e2' <- e2 env 
  opEq (==) (e1', t1) (e2', t2)
  
denote (OpCmp Neq (e1, t1) (e2, t2)) env = do
  e1' <- e1 env 
  e2' <- e2 env 
  opEq (/=) (e1', t1) (e2', t2)

denote (OpCmp Lt (e1, t1) (e2, t2)) env = do
  e1' <- e1 env 
  e2' <- e2 env 
  opCmp (<) (e1', t1) (e2', t2)

denote (OpCmp Lte (e1, t1) (e2, t2)) env = do
  e1' <- e1 env 
  e2' <- e2 env 
  opCmp (<=) (e1', t1) (e2', t2)

denote (OpCmp Gt (e1, t1) (e2, t2)) env = do
  e1' <- e1 env 
  e2' <- e2 env 
  opCmp (>) (e1', t1) (e2', t2)

denote (OpCmp Gte (e1, t1) (e2, t2)) env = do
  e1' <- e1 env 
  e2' <- e2 env 
  opCmp (>=) (e1', t1) (e2', t2)
