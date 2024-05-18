module Entity.Syntax where
import Fun.Syntax (FDecl, FunName)
import Syntax (Address)

type EName = String --entity name
type PName = String --property name

data Entity e = EntityDecl EName [PName] [FDecl e]
    deriving Functor
    
data EntityEnv e = EEnv EName [((PName, ScopedType), Address)]
    deriving Functor

data ScopedType = Property | Function
    deriving Eq



-- data Program e
--     = Program [FDecl e] [Entity e] e
--     deriving Functor