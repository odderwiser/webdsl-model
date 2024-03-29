module Fun.Syntax where
import Eval.Syntax (VName)
import Utils.Free

type FunName = String

data FDecl e 
    = FDecl FunName [VName] e
    deriving Functor

data Fun e 
    = Return e
    | FCall FunName [e]
    deriving Functor

data Program e
    = Program [FDecl e] e
    deriving Functor