module Expr.Denotation where
import Expr.Syntax
import Bool.Syntax as B
import Utils.Composition (type (<:), type (<) (injV), projV)
import Utils.Free (Free)
import Utils.Denote (Env)
import qualified Arith.Denotation as A
import qualified Bool.Denotation as B
import Syntax (Type (..))
import qualified Arith.Syntax as A
import Foreign (fromBool)

op :: (Functor f, Num a, Bool < v)
  => (a -> a -> Bool) -> a -> a -> Free f v
op operand e1 e2 = return 
  $ injV 
  $ operand e1 e2

opAr :: (Functor f, Num a, Int < v, Bool < v) 
  => (a -> a -> Bool) -> v -> v -> Free f v
opAr operand exp1 exp2 = 
  case (projV exp1, projV exp2) of
  (Just e1' :: Maybe Int, Just e2' :: Maybe Int) -> 
    op operand (fromIntegral e1') (fromIntegral e2')

opB :: (Functor f, Num a, Bool < v) 
  => (a -> a -> Bool) -> v -> v -> Free f v
opB operand exp1 exp2 = 
  case (projV exp1, projV exp2) of
    (Just e1', Just e2') -> 
      op operand (fromBool e1') (fromBool e2')

opEq :: (Functor f, Num a, Bool < v, Int < v)
  => (a -> a -> Bool) -> (v, Type) -> (v, Type) 
  -> Free f v
opEq operand (exp1, type1) (exp2, type2) = 
  case (type1, type2) of
  (Int,  Int)  -> opAr operand exp1 exp2
  (Bool, Bool) -> opB  operand exp1 exp2
      
opCmp :: (Functor f, Num a, Bool < v, Int < v)
  => (a -> a -> Bool) -> (v, Type) -> (v, Type) 
  -> Free f v
opCmp operand (exp1, _) (exp2, _) = opAr operand exp1 exp2

denote :: (Functor eff, Int < v, Bool < v)
  => Expr (Env eff v -> Free eff v)
  -> Env eff v -> Free eff v

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
