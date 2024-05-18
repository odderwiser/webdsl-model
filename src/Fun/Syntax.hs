module Fun.Syntax where
import Eval.Syntax (VName)
import Utils.Free

type FunName = String

data FDecl e 
    = FDecl FunName [VName] e
    deriving Functor

data Fun name e 
    = Return e
    | FCall name [e]
    deriving Functor