module Actions.Modules.Str.Syntax where
import Utils.Composition
import Utils.Fix
import Data.Maybe (fromJust)

data LitStr e = Lit String
    deriving (Functor, Eq)

instance Show (LitStr a) where
  show :: LitStr a -> String
  show (Lit v) = show v 

lit :: (LitStr <: v) => String -> Fix v 
lit =  injF . Lit

data Str e 
    = LitS String 
    | Add e e 
    deriving Functor

str :: (Str <: f) => String -> Fix f
str =  injF . LitS

add :: (Str <: f) => Fix f -> Fix f -> Fix f 
add left right = injF $ Add left right

projS :: (LitStr <: g) => Fix g -> String
projS elem = case fromJust (projF elem) of
  (Lit int) -> int