module Entity.Syntax where
import Fun.Syntax (FDecl, FunName)
import Syntax (Address)
import Utils.Fix
import Utils.Composition (type (<:))

type EName = String --entity name
type PName = String --property name

data EntityDef e = EDef EName [PName] [FDecl e]
    deriving Functor
    
data EntityDecl e = EDecl EName e -- e can be address or the Env
    deriving Functor

data AddressBox e = Box Address
    deriving Functor 

getAddress :: (EntityDecl <: g, AddressBox <: g) => Fix g -> Address
getAddress entity = case projF entity of
    Just (EDecl name address) -> case projF address of
        Just (Box address) -> address

getEnv entity = case projF entity of
    Just (EDecl name address) -> address