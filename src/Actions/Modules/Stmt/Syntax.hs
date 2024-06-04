module Actions.Modules.Stmt.Syntax where
import Actions.Modules.Eval.Syntax (VName)
import Utils.Composition (type (<:))
import Utils.Fix (Fix, injF)

data Stmt e = S e e -- Statement concat
    deriving Functor

cons :: (Stmt <: f) => Fix f -> Fix f -> Fix f
cons head tail = injF $ S head tail

data Loop e
    = ForCol VName e e [Filter e] -- for all elements in collection
    -- | ForEnt VName EName [Filter e] -- for all entities of type EName
    | ForArith VName e e e -- for numbers in range from e2 to e3
    | While e e
    deriving Functor

forAll :: (Loop <: f) 
    => VName -> Fix f -> Fix f -> [Filter (Fix f)] -> Fix f
forAll elemId col loopBody filters = injF $ ForCol elemId col loopBody filters

forRange :: (Loop <: f) 
    => VName -> Fix f -> Fix f -> Fix f -> Fix f
forRange elemId start end loopBody = injF $ ForArith elemId start end loopBody

while :: (Loop <: f) => Fix f -> Fix f -> Fix f
while condition loopBody = injF $ While condition loopBody

type IsAscending = Bool  

data Filter e 
  = Where e
  | OrdBy e IsAscending -- order by e (asc = True, desc = False)
  | Limit e
  | Offset e
  deriving Functor

where' :: (Filter <: f) => Fix f -> Fix f 
where' = injF . Where

orderBy :: (Filter <: f) => Fix f -> IsAscending -> Fix f
orderBy exp isAscending = injF $ OrdBy exp isAscending

limit :: (Filter <: f) => Fix f -> Fix f
limit = injF . Limit

offset :: (Filter <: f) => Fix f -> Fix f
offset = injF . Offset