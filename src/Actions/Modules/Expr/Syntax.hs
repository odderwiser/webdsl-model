module Actions.Modules.Expr.Syntax where
import Syntax (Type)

data Cmp = Eq | Neq | Lt | Lte | Gt | Gte 

data Expr e 
    = OpCmp Cmp e e 
    deriving Functor
