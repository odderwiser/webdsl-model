module Entity.Syntax where
--import Actions.Syntax (F( FDecl, FunName), Ev(VName, Eval))
import Syntax (Address, Type)
import Utils.Fix
import Utils.Composition (type (<:))
import Data.Maybe (fromJust)
import Actions.Modules.Eval.Syntax
import Actions.Modules.Fun.Syntax

type EName = String --entity name
type PName = String --property name

-- global only
data EntityDef e = EDef EName [(PName, Type)] [FDecl e]
    deriving (Functor, Eq)

data GlobalVar e = VarDecl VName e 

-- inside program too

data EntityDecl e = EDecl EName [(PName, e)] --- e is type if unevaled and address if evaled
    deriving (Functor, Eq)

data Entity e 
    = PropAccess e PName
    | ECall e FunName [e]
    | PropAssign e PName e
    | PVar PName
    deriving Functor

data LitAddress e = Box Address
    deriving (Functor, Eq) 


projParams :: (LitAddress <: g, EntityDecl <: g) => Fix g -> [(PName, Address)]
projParams entity = case projF entity of
    Just (EDecl _ addresses) -> map 
        (\(a, b) -> (a, case fromJust $ projF b of Box b' -> b'))
        addresses

projEName entity = case projF entity of
    Just (EDecl name _) -> name

-- projEntity :: (EntityDecl param <: g) => Fix g -> EntityDecl param (Fix g)
-- projEntity = fromJust . projF