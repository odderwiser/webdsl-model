module Actions.Modules.Entity.Syntax where
import Definitions.Entity.Syntax
import Definitions.Fun.Syntax
import Syntax (Address)
import Data.Maybe (fromJust)
import Utils.Composition
import Utils.Fix
import GHC.Generics (Generic)
import Actions.Values

data EntityDecl e = EDecl EName [(PName, e)] --- e is type if unevaled and address if evaled
    deriving (Functor, Eq, Show, Generic)

eDecl :: (EntityDecl <: f) => EName ->  [(PName, Fix f)] -> Fix f
eDecl eName params = injF $ EDecl eName params

-- data LitV v e = Box v
--     deriving (Functor, Eq, Show)




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

projParams :: (Lit Address <: g) => Maybe (EntityDecl (Fix g)) -> [(PName, Fix g)]
projParams entity = case entity of
    Just (EDecl _ addresses) -> addresses

projEName (EDecl name _) = name

projEntity :: (EntityDecl <: f) => Fix f -> EntityDecl (Fix f)
projEntity = fromJust . projF