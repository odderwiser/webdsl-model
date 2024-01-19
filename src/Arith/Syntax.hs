module Arith.Syntax where
import Utils.Composition
import Utils.Fix
import Utils.Denote (Literal (coerce))

data LitAr a = Lit Int
  deriving Functor

instance Literal LitAr where
  coerce :: LitAr a -> LitAr b
  coerce (Lit a) = Lit a 

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
