module Bool.Syntax where

data LitB = Lit Bool 

data OpB = Or | And 

data Boolean e = LitB LitB 
    | OpB OpB e e  
    | If e e e
    deriving Functor
