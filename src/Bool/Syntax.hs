module Bool.Syntax where
import Utils.Composition
import Utils.Fix

data LitB = Lit Bool 

data OpB = Or | And 

data Boolean e = LitB LitB 
    | OpB OpB e e  
    | If e e e
    deriving Functor

instance (Boolean <: g) 
    => BinaryInject Boolean g OpB where
  bin :: OpB 
    -> g (Fix g)
    -> g (Fix g)  
    -> Boolean (Fix g)
  bin op left right = 
    OpB op (injF left) (injF right)
