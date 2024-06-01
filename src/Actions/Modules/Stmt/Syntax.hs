module Actions.Modules.Stmt.Syntax where
import GHC.OldList (elemIndex)
import Actions.Modules.Eval.Syntax (VName)

data Stmt e = S e e -- Statement concat
    deriving Functor

data Loop e
    = ForCol VName e e [Filter e] -- for all elements in collection
    -- | ForEnt VName EName [Filter e] -- for all entities of type EName
    | ForArith VName e e e -- for numbers in range from e2 to e3
    | While e e
    deriving Functor

data Filter e 
  = Where e
  | OrdBy e Bool -- order by e (asc = True, desc = False)
  | Limit e
  | Offset e
  deriving Functor