module Actions.Modules.Fun.Syntax where
import Actions.Modules.Eval.Syntax (VName)

type FunName = String

data FDecl e 
    = FDecl FunName [VName] e
    deriving (Functor, Eq)

data Fun e 
    = Return e
    | FCall FunName [e]
    deriving Functor