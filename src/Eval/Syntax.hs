module Eval.Syntax where
import Syntax (Type)

type VName = String

data Eval e 
    = Var       VName 
    | VDecl     VName        e
    | VValDecl  VName e Type e
    | VAssign   VName e Type
    deriving Functor
