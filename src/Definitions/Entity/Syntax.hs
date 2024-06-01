module Definitions.Entity.Syntax where
import Syntax (Type)
import Definitions.Fun.Syntax (FDecl)

type EName = String --entity name
type PName = String --property name

-- global only
data EntityDef e = EDef EName [(PName, Type)] [FDecl e]
    deriving (Functor, Eq)
