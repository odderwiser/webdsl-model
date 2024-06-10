module Definitions.Entity.Syntax where
import Syntax (Type)
import Definitions.Fun.Syntax (FDecl)

type EName = String --entity name
type PName = String --property name
type Props = [(PName, Type)]
data ImplicitProp = Id 
    deriving (Eq, Show)-- extendable
type ImplicitProps e = [ImplicitProp] -- if Nothing, do the implicit operation. overloading not yet supported 
    
-- global only
data EntityDef e = EDef EName Props (ImplicitProps e) [FDecl e]
    deriving (Functor, Eq, Show)
