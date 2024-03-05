module Expr.Syntax where
import Bool.Syntax (Boolean)
import Arith.Syntax (Arith)

data Cmp = Eq | Neq | Lt | Lte | Gt | Gte | Is  

data Expr e 
    = Arith (Arith e)
    | Boolean (Boolean e) 
    | OpCmp Cmp e e  
    deriving Functor