module Actions.Modules.Bool.Syntax where
import Utils.Composition ( type (<:) )
import Utils.Fix ( injF, Fix, projF )
import Data.Maybe (fromJust)

--- VALUES

data LitBool e = Lit Bool
  deriving (Functor, Eq)

instance Show (LitBool a) where
  show :: LitBool a -> String
  show (Lit v) = show v 

--smart constructors

lit :: (LitBool <: v) => Bool -> Fix v
lit = injF . Lit 

--- SYNTAX

data OpB = Or | And 

data Boolean e = LitB Bool
    | OpB OpB e e  
    | If e e e
  deriving Functor

-- smart constructors 

bin :: (Boolean <: g) => 
  OpB -> Fix g -> Fix g -> Fix g
bin op left right = injF $ OpB op left right

or :: (Boolean <: g) =>  Fix g -> Fix g -> Fix g
or = bin Or

and :: (Boolean <: g) =>  Fix g -> Fix g -> Fix g
and = bin And

if' :: (Boolean <: g) => 
  Fix g -> Fix g -> Fix g  
  -> Fix g
if' a b c = injF $ If a b c 

injB :: (Boolean <: f) => Bool -> Fix f
injB =  injF . LitB

true :: (Boolean <: f) => Fix f
true = injF $ LitB True

false :: (Boolean <: f) => Fix f
false = injF $ LitB False

-- projBool :: forall f. (LitBool <: f) => Fix f -> Bool
projBool elem = case fromJust (projF elem) of
  (Lit bool) -> bool