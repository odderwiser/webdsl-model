module Eval.Syntax where
import Syntax (Type)

type VName = String

data Eval e 
    = Var       VName           -- use a variable
    | VDecl     VName        e  -- declare but not initialise, with continuation
    | VValDecl  VName e Type e  -- declare and initialise variable
    | VAssign   VName e Type    -- assign value to an existing variable
    deriving Functor
