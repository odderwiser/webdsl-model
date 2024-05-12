module Col.Syntax where
import Syntax (Type)
import Utils.Composition
import Data.Maybe (mapMaybe)

data Col e 
  = LitC [e]  
  | OpIn e e
  -- | Add e e this is perhaps a statement
    deriving Functor

lit :: [a] -> Col a
lit = LitC 