module Actions.Modules.Eval.Syntax where
import Syntax (Type)
import Utils.Fix
import Utils.Composition

type VNames = [VName]
type VName = String

data Eval e 
    = Var       VName      -- use a variable
    | VDecl     VName   e  -- declare but not initialise, with continuation
    | VValDecl  VName e e  -- declare and initialise variable
    | VAssign   VName e    -- assign value to an existing variable
    deriving Functor

injVar :: (Eval <: f) => VName -> Fix f
injVar = injF . Var