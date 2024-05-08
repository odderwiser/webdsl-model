module Arith.Syntax where
import Utils.Composition ( type (<:) )
import Utils.Fix ( injF, BinaryInject(..), Fix )

data OpArith = Add | Div | Sub | Mul | Mod

data LitInt v = Lit Int
  deriving Functor

data Arith e = LitAr (LitInt e) 
    | OpArith OpArith e e
    deriving Functor

instance (Arith <: g) => BinaryInject Arith g OpArith where
  bin :: (Arith <: g) 
    => OpArith 
    -> g (Fix g) 
    -> g (Fix g) 
    -> Arith (Fix g)
  bin op left right = OpArith op (injF left) (injF right)

lit :: Int -> Arith e
lit = LitAr . Lit 
