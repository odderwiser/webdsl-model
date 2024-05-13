module Entity.Syntax where
import Fun.Syntax (FDecl)
import Syntax (Address)

type EName = String --entity name
type PName = String --property name

data Entity e = Entity EName (Maybe (Entity e)) [PName] [FDecl e]
    deriving Functor
    
type EntityEnv = (EName, [(PName, Address)])

data Program e
    = Program [FDecl e] [Entity e] e
    deriving Functor