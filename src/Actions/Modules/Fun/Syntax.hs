module Actions.Modules.Fun.Syntax where
import Definitions.Fun.Syntax 
import Utils.Composition (type (<:))
import Utils.Fix (Fix, injF)

data Fun e 
    = Return e
    | FCall FunName [e]
    deriving Functor

return' :: (Fun <: g) => Fix g -> Fix g 
return' = injF . Return

funCall :: (Fun <: g) => FunName -> [Fix g] -> Fix g
funCall fName args = injF $ FCall fName args