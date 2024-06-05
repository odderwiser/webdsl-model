module Actions.Modules.Entity.Syntax where
import Definitions.Entity.Syntax
import Definitions.Fun.Syntax
import Utils
import Syntax (Address)
import Data.Maybe (fromJust)

data EntityDecl e = EDecl EName [(PName, e)] --- e is type if unevaled and address if evaled
    deriving (Functor, Eq, Show)

eDecl :: (EntityDecl <: f) => EName ->  [(PName, Fix f)] -> Fix f
eDecl eName params = injF $ EDecl eName params

data LitAddress e = Box Address
    deriving (Functor, Eq, Show)

data Entity e 
    = PropAccess e PName
    | ECall e FunName [e]
    | PropAssign e PName e
    | PVar PName
    deriving Functor 

pAccess :: (Entity <: f) => Fix f -> PName -> Fix f 
pAccess entity pName = injF $ PropAccess entity pName

eCall :: (Entity <: f) => Fix f -> FunName -> [ Fix f] -> Fix f
eCall entity fName args = injF $ ECall entity fName args

pVar :: (Entity <: f) => PName -> Fix f
pVar = injF . PVar

projParams :: (LitAddress <: g, EntityDecl <: g) => Fix g -> [(PName, Address)]
projParams entity = case projF entity of
    Just (EDecl _ addresses) -> map 
        (\(a, b) -> (a, case fromJust $ projF b of Box b' -> b'))
        addresses

projEName entity = case projF entity of
    Just (EDecl name _) -> name