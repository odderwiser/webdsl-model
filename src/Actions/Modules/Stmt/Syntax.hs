module Actions.Modules.Stmt.Syntax where
import Actions.Modules.Eval.Syntax (VName)
import Utils.Composition (type (<:))
import Utils.Fix (Fix, injF)
import Templates.Modules.Lift.Syntax (Weaken (Weaken))
import Data.Bifunctor (Bifunctor (bimap))

data Stmt e = S e e -- Statement concat
    deriving Functor

cons :: (Stmt <: f) => Fix f -> Fix f -> Fix f
cons head tail = injF $ S head tail

data Loop t a
    = ForCol VName a t [Filter a] -- for all elements in collection
    -- | ForEnt VName EName [Filter e] -- for all entities of type EName
    | ForArith VName a a t -- for numbers in range from e2 to e3
    | While a t
    deriving Functor

instance Bifunctor Loop where
    bimap g f  (ForCol name a t filter) = f <$> ForCol name a (g t) filter
    bimap g f (ForArith name a a' t) = f <$> ForArith name a a' (g t)
    bimap g f (While a t) = f <$> While a (g t)


forAll :: (Weaken Loop <: f)
    => VName -> Fix f -> Fix f -> [Filter (Fix f)] -> Fix f
forAll elemId col loopBody filters = injF $ Weaken $ ForCol elemId col loopBody filters

forRange :: (Weaken Loop <: f)
    => VName -> Fix f -> Fix f -> Fix f -> Fix f
forRange elemId start end loopBody = injF $ Weaken $ ForArith elemId start end loopBody

while :: (Weaken Loop <: f) => Fix f -> Fix f -> Fix f
while condition loopBody = injF $ Weaken $ While condition loopBody

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