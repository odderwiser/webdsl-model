module Eval.Syntax where
import Syntax (Type)
import Utils.Fix
import Utils.Composition

type VNames = [VName]
type VName = String

data Eval e 
    = Var       VNames           -- use a variable
    | VDecl     VNames        e  -- declare but not initialise, with continuation
    | VValDecl  VNames e Type e  -- declare and initialise variable
    | VAssign   VNames e Type    -- assign value to an existing variable
    deriving Functor

injVar :: (Eval <: f) => VName -> Fix f
injVar v = injF . Var $ [v]