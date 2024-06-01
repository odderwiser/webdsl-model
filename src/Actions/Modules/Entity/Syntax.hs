module Actions.Modules.Entity.Syntax where
import Definitions.Entity.Syntax
import Definitions.Fun.Syntax
import Utils
import Syntax (Address)
import Data.Maybe (fromJust)

data EntityDecl e = EDecl EName [(PName, e)] --- e is type if unevaled and address if evaled
    deriving (Functor, Eq, Show)

data Entity e 
    = PropAccess e PName
    | ECall e FunName [e]
    | PropAssign e PName e
    | PVar PName
    deriving Functor

data LitAddress e = Box Address
    deriving (Functor, Eq, Show) 


projParams :: (LitAddress <: g, EntityDecl <: g) => Fix g -> [(PName, Address)]
projParams entity = case projF entity of
    Just (EDecl _ addresses) -> map 
        (\(a, b) -> (a, case fromJust $ projF b of Box b' -> b'))
        addresses

projEName entity = case projF entity of
    Just (EDecl name _) -> name