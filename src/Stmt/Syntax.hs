module Stmt.Syntax where
import GHC.OldList (elemIndex)
import Eval.Syntax (VName)
import Entity.Syntax (EName)

data Stmt e = S e e
    | ForC VName e e [Filter e]
    | ForE VName EName [Filter e]
    | ForA VName e e e [Filter e]
    | While e e
    deriving Functor

data Filter e 
  = Where e
  | OrdBy e Bool -- order by e (asc = True, desc = False)
  | Limit e
  | Offset e
  deriving Functor