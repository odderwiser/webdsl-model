module Definitions.Entity.Syntax where
import Syntax (Type)
import Definitions.Fun.Syntax (FDecl)
import Utils.Composition (type (<:) (inj), type (<::), inj')
import Templates.Modules.Lift.Syntax (LiftE (LiftE))
import Actions.Modules.Phases.Syntax

type EName = String --entity name
type PName = String --property name
type Props = [(PName, Type)]
data ImplicitProp = Id
    deriving (Eq, Show)-- extendable
type ImplicitProps e = [ImplicitProp] -- if Nothing, do the implicit operation. overloading not yet supported 

-- global only
data EntityDef e = EDef EName Props (ImplicitProps e) [FDecl e] [VTuple e]
    deriving (Functor, Eq, Show)

eDef :: (EntityDef <: f) => EName -> Props -> [FDecl e] -> [VTuple e] -> f e
eDef name props f v = inj $ EDef name props [Id] f v

eDef' :: (LiftE (EntityDef) <:: f) => EName -> Props -> [FDecl a] -> [VTuple a] -> f t a
eDef' name props f v = inj' $ LiftE $ EDef name props [Id] f v