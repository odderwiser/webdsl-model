module Entity.Syntax where
import Fun.Syntax (FDecl)
import Syntax (Address)

type EName = String
type PName = String

data Entity e = Entity EName (Maybe (Entity e)) [PName] [FDecl e]
    deriving Functor
    
type EntityEnv = [(PName, Address)]

data Dot e = Param EName PName

data Program e
    = Program [FDecl e] [Entity e] e
    deriving Functor