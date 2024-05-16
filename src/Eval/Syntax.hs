module Eval.Syntax where
import Syntax (Type)
import Utils.Fix
import Utils.Composition

type VNames = [VName]
type VName = String

data Eval name e 
    = Var       name      -- use a variable
    | VDecl     name   e  -- declare but not initialise, with continuation
    | VValDecl  name e e  -- declare and initialise variable
    | VAssign   name e    -- assign value to an existing variable
    deriving Functor

injVar :: (Eval VName <: f) => VName -> Fix f
injVar = injF . Var