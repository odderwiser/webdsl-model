module Expr.Denotation where
import Expr.Syntax
import Arith.Syntax as A
import Bool.Syntax as B
import Utils.Composition (type (<:), type (<) (injV), projV)
import Utils.Free (Free)
import Utils.Denote (Env)
import qualified Arith.Denotation as A
import qualified Bool.Denotation as B
import Syntax (Type (..))
import qualified Arith.Syntax as A
import Foreign (fromBool)
import Utils.Fix

op :: (Functor f, Num a,  LitBool <: v)
  => (a -> a -> Bool) -> a -> a -> Free f (Fix v)
op operand e1 e2 = return 
  $ injF $ B.Lit  
  $ operand e1 e2

opAr :: (fix ~ Fix v, Functor f, Num a, LitInt <: v, LitBool <: v) 
  => (a -> a -> Bool) -> fix -> fix -> Free f fix
opAr operand exp1 exp2 = 
  case (projF exp1, projF exp2) of
  (Just (A.Lit e1'), Just (A.Lit e2')) -> 
    op operand (fromIntegral e1') (fromIntegral e2')

opB :: (fix ~ Fix v, Functor f, Num a, LitInt <: v, LitBool <: v) 
  => (a -> a -> Bool) -> fix -> fix -> Free f fix
opB operand exp1 exp2 = 
  case (projF exp1, projF exp2) of
    (Just (B.Lit e1'), Just (B.Lit e2')) -> 
      op operand (fromBool e1') (fromBool e2')

opEq :: (fix ~ Fix v, Functor f, Num a, LitInt <: v, LitBool <: v)
  => (a -> a -> Bool) -> (fix, Type) -> (fix, Type) 
  -> Free f fix
opEq operand (exp1, type1) (exp2, type2) = 
  case (type1, type2) of
  (Int,  Int)  -> opAr operand exp1 exp2
  (Bool, Bool) -> opB  operand exp1 exp2
      
opCmp :: (fix ~ Fix v, Functor f, Num a, LitInt <: v, LitBool <: v)
  => (a -> a -> Bool) -> (fix, Type) -> (fix, Type) 
  -> Free f fix
opCmp operand (exp1, _) (exp2, _) = opAr operand exp1 exp2

denote :: (fix ~ Fix v, Functor eff, LitInt <: v, LitBool <: v)
  => Expr (Env eff fix -> Free eff fix)
  -> Env eff fix -> Free eff fix

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
