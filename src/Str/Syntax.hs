module Str.Syntax where
import Utils.Composition
import Utils.Fix

data LitStr e = Lit String
    deriving Functor

data Str e 
    = LitS String 
    | Add e e 
    deriving Functor

injS :: (Str <: f) => String -> Fix f
injS =  injF . LitS