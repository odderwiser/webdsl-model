module Col.Syntax where
import Syntax (Type)
import Utils.Composition
import Utils.Fix

data Col e 
  = LitC [e]  
  | OpIn e e
  -- | Add e e this is perhaps a statement
    deriving Functor

lit :: [a] -> Col a
lit = LitC 

injC :: (Col <: f) => [Fix f] -> Fix f
injC =  injF . LitC