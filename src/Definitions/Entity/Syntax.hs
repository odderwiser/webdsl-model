module Definitions.Entity.Syntax where
import Syntax (Type)
import Definitions.Fun.Syntax (FDecl)
import Utils.Composition (type (<:) (inj), type (<::), inj')
import Templates.Modules.Lift.Syntax (LiftE (LiftE))

type EName = String --entity name
type PName = String --property name
type Props = [(PName, Type)]
data ImplicitProp = Id
    deriving (Eq, Show)-- extendable
type ImplicitProps e = [ImplicitProp] -- if Nothing, do the implicit operation. overloading not yet supported 

-- global only
data EntityDef e = EDef EName Props (ImplicitProps e) [FDecl e]
    deriving (Functor, Eq, Show)

eDef :: (EntityDef <: f) => EName -> Props -> [FDecl e] -> f e
eDef name props f = inj $ EDef name props [Id] f

eDef' :: (LiftE (EntityDef) <:: f) => EName -> Props -> [FDecl a] -> f t a
eDef' name props f = inj' $ LiftE $ EDef name props [Id] f