module Actions.Modules.Eval.Syntax where
import Syntax (Type)
import Utils.Fix
import Utils.Composition
import Data.Bifunctor (Bifunctor, bimap)
import Data.UUID.V3 (namespaceDNS)
import Templates.Modules.Lift.Syntax (Weaken (..))

type VNames = [VName]
type VName = String

type Eval = Weaken EvalT 

data EvalT e f 
    = Var       VName      -- use a variable
    | VDecl     VName   f  -- declare but not initialise, with continuation
    | VValDecl  VName e f  -- declare and initialise variable
    | VAssign   VName e    -- assign value to an existing variable
    deriving Functor

instance Bifunctor EvalT where
    bimap f g (Var name) = Var name 
    bimap f g (VDecl name c) = VDecl name $ g c 
    bimap f g (VValDecl name val c) = VValDecl name (f val) (g c)
    bimap f g (VAssign name value) = VAssign name (f value)

var :: (Eval <: f) => VName -> Fix f
var = injF . Weaken . Var

varDecl :: (Eval <: g) => VName -> Fix g -> Fix g
varDecl name k = injF $ Weaken $ VDecl name k

varInit :: (Eval <: g) => VName -> Fix g -> Fix g -> Fix g
varInit name exp k = injF $ Weaken $ VValDecl name exp k

varAssign :: (Weaken (EvalT) <: g) => VName -> Fix g -> Fix g
varAssign name exp = injF $ Weaken $ VAssign name exp