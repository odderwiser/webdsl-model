module Arith.Syntax where
import Utils.Composition
import Utils.Fix

data LitAr = Lit Int 

data OpArith = Add | Div | Sub | Mul | Mod

data Arith e = LitAr LitAr 
    | OpArith OpArith e e  
    deriving Functor

instance (Arith <: g) => BinaryInject Arith g OpArith where
  bin :: (Arith <: g) => OpArith -> 
    Arith (Fix g) -> Arith (Fix g) -> Fix g
  bin op left right = injF $ 
    OpArith op (injF left) (injF right)
