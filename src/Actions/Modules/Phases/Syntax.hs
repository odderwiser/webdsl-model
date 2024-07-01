module Actions.Modules.Phases.Syntax where
import Syntax (Type)
import Utils.Composition
import Utils.Fix



data VTuple e = Validate e String [String]
    deriving (Functor, Eq, Show)

data Redirect e = Redirect String [(e, Type)]
    deriving Functor

redirectS :: (Redirect <: f) => String -> Fix f
redirectS string = injF $ Redirect string []