module Arith.Syntax where
import Utils.Composition
import Utils.Fix

data LitAr a = Lit Int
  deriving Functor

data OpArith = Add | Div | Sub | Mul | Mod

data Arith e = LitAr (LitAr e)
    | OpArith OpArith e e  
    deriving Functor

instance (Arith <: g) => BinaryInject Arith g OpArith where
  bin :: OpArith 
    -> g (Fix g) 
    -> g (Fix g) 
    -> Arith (Fix g)
  bin op left right =
    OpArith op (injF left) (injF right)
