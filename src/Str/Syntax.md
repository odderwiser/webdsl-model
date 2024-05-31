-- module Str.Syntax where
-- import Utils.Composition
-- import Utils.Fix
-- import Data.Maybe (fromJust)

-- data LitStr e = Lit String
--     deriving (Functor, Eq)

-- data Str e 
--     = LitS String 
--     | Add e e 
--     deriving Functor

-- injS :: (Str <: f) => String -> Fix f
-- injS =  injF . LitS

-- projS :: (LitStr <: g) => Fix g -> String
-- projS elem = case fromJust (projF elem) of
--   (Lit int) -> int