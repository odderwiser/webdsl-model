module Actions.Modules.Col.Syntax where
import Actions.Modules.Eval.Syntax (VName)
import Actions.Modules.Stmt.Syntax (Filter)
import Actions.Modules.Bool.Syntax (OpB)

import Utils.Composition
import Utils.Fix

import Data.Maybe (fromJust)

data Col e 
  = LitC [e]                   -- List literal : []
  | OpIn e e                   -- Contains: e1 in e2
  | LComp (Maybe OpB) e VName e [Filter e] -- List Comprehension: (Maybe AND / OR) [ e1 | VName : type in e2 filter ]. Syntactic sugar for ForLoop?
  deriving Functor

injC :: (Col <: f) => [Fix f] -> Fix f
injC =  injF . LitC

in' :: (Col <: g) 
  => Fix g -> Fix g -> Fix g
in' left right = injF $ OpIn left right

lComp :: (Col <: g) 
  => Maybe OpB -> Fix g -> VName -> Fix g -> [Filter (Fix g)] -> Fix g
lComp op exp name col filters = injF $ LComp op exp name col filters

projC :: ([] <: g) => Fix g -> [Fix g]
projC col = fromJust (projF col)