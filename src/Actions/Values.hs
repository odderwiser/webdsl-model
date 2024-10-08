module Actions.Values where
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Utils.Composition
import Utils.Fix
import Utils.Free

data Lit v e = V v | Box v 
    deriving (Functor, Eq, Show, Generic)

instance (ToJSON v) => ToJSON (Lit v (Fix v'))
instance (FromJSON v) => FromJSON (Lit v (Fix v'))

unbox a = case projF a of
    (Just (Box a')) -> a'   
    (Just (V a')) -> a'   

unbox' a = case projF a of
    (Just (Box a')) -> a'   
    (Just (V a')) -> a' 

box :: (Lit v <: f) => v -> Fix f
box = injF . Box

boxV :: (Lit v <: f) => v -> Fix f
boxV = injF . V

v :: (Functor f, Lit a <: v) => a -> Free f (Fix v)
v = return . injF . V

unV :: Lit a e -> a
unV (V a) = a

unV' :: Lit a e -> a
unV' (V a) = a

projV :: (Lit a <: g) => Fix g -> Maybe a
projV (In fix) = unV <$> proj fix 

projV'' :: (Lit a <: g) => Fix g -> Maybe a
projV'' (In fix) = unV' <$> proj fix 

showValue :: forall a v. (Show a, Lit a <: v) => Fix v -> String
showValue = show . (unbox' :: Fix v -> a) 

data Null e = Null
    deriving (Functor, Eq, Show, Generic)

instance ToJSON (Null e)
instance FromJSON (Null e)
null :: (Null <: v) => Fix v 
null = injF Null

type Uuid = String
