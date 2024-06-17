module Actions.Modules.Eval.Syntax where
import Syntax (Type)
import Utils.Fix
import Utils.Composition
import Data.Bifunctor (Bifunctor, bimap)
import Data.UUID.V3 (namespaceDNS)
import Templates.Modules.Lift.Syntax (Weaken (..))

type VNames = [VName]
type VName = String

data Eval e 
    = Var       VName      -- use a variable
    | VDecl     VName    e  -- declare but not initialise, with continuation
    | VValDecl  VName e  e -- declare and initialise variable
    | VAssign   VName e    -- assign value to an existing variable
    deriving Functor

var :: (Eval <: f) => VName -> Fix f
var = injF . Var

varDecl :: (Eval <: g) => VName -> Fix g -> Fix g
varDecl name k = injF $ VDecl name k

varInit :: (Eval <: g) => VName -> Fix g -> Fix g -> Fix g
varInit name exp k = injF $ VValDecl name exp k

varAssign :: (Eval <: g) => VName -> Fix g -> Fix g
varAssign name exp = injF $ VAssign name exp