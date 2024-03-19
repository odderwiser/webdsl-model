module Fun.Syntax where
import Eval.Syntax (VName)
import Utils.Denote (Env)

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