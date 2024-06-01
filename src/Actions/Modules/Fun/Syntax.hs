module Actions.Modules.Fun.Syntax where
import Definitions.Fun.Syntax 

data Fun e 
    = Return e
    | FCall FunName [e]
    deriving Functor