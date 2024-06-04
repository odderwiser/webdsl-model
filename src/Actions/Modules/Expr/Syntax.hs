module Actions.Modules.Expr.Syntax where
import Utils.Composition (type (<:))
import Utils.Fix (Fix, injF)

data Cmp = Eq | Neq | Lt | Lte | Gt | Gte 

data Expr e 
    = OpCmp Cmp e e 
    deriving Functor

bin :: (Expr <: g) => 
  Cmp -> Fix g -> Fix g -> Fix g
bin op left right = injF $ OpCmp op left right