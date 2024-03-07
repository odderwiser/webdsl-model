module Expr.Syntax where
import Bool.Syntax (Boolean)
import Arith.Syntax (Arith)
import Syntax (Type)

data Cmp = Eq | Neq | Lt | Lte | Gt | Gte 

data Expr e 
    = Arith (Arith e)
    | Boolean (Boolean e) 
    | OpCmp Cmp (e, Type) (e, Type)  
    deriving Functor
