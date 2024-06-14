module Actions.Values where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON)
import Utils.Composition
import Utils.Fix

data Lit v e = Box v
    deriving (Functor, Eq, Show, Generic)

instance (ToJSON v) => ToJSON (Lit v (Fix v'))

unbox a = case projF a of
    (Just (Box a')) -> a'   

box :: (Lit v <: f) => v -> Fix f
box = injF . Box

showValue :: forall a v. (Show a, Lit a <: v) => Fix v -> String
showValue = show . (unbox :: Fix v -> a) 

