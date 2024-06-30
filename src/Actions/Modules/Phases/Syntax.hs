module Actions.Modules.Phases.Syntax where
import Syntax (Type)


data VTuple e = Validate e String [String]
    deriving (Functor, Eq, Show)

data Redirect e = Redirect String [(e, Type)]
    deriving Functor