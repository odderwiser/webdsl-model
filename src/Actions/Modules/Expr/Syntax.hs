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

eq :: (Expr <: g) => Fix g -> Fix g -> Fix g
eq = bin Eq

neq :: (Expr <: g) => Fix g -> Fix g -> Fix g
neq = bin Neq

lt :: (Expr <: g) => Fix g -> Fix g -> Fix g
lt = bin Lt

lte :: (Expr <: g) => Fix g -> Fix g -> Fix g
lte = bin Lte

gt :: (Expr <: g) => Fix g -> Fix g -> Fix g
gt = bin Gte 

gte :: (Expr <: g) => Fix g -> Fix g -> Fix g
gte = bin Gte

