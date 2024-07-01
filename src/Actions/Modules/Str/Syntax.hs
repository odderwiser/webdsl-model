module Actions.Modules.Str.Syntax where
import Utils.Composition
import Utils.Fix
import Data.Maybe (fromJust)
import Actions.Values

type LitStr = Lit String

data Str e 
    = LitS String 
    | Add e e 
    | Length e
    deriving Functor

str :: (Str <: f) => String -> Fix f
str =  injF . LitS

add :: (Str <: f) => Fix f -> Fix f -> Fix f 
add left right = injF $ Add left right

length :: (Str <: f) => Fix f -> Fix f
length  str = injF $ Length str