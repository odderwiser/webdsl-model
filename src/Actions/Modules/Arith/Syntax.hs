module Actions.Modules.Arith.Syntax where
import Utils.Composition ( type (<:) )
import Utils.Fix ( injF, Fix, projF )
import Data.Maybe (fromJust)

-- VALUES

data LitInt v = Lit Int
  deriving (Functor, Eq)

instance Show (LitInt a) where
  show :: LitInt a -> String
  show (Lit v) = show v 

-- smart constructor

lit :: (LitInt <: v) => Int -> Fix v
lit = injF . Lit 

--- SYNTAX

data OpArith = Add | Div | Sub | Mul | Mod

data Arith e = LitAr Int 
    | OpArith OpArith e e
    deriving Functor

-- smart constructors

bin :: (Arith <: g) 
  => OpArith -> Fix g -> Fix g -> Fix g
bin op left right = injF $ OpArith op left right

injA :: (Arith <: f) => Int -> Fix f
injA =  injF . LitAr

projArith :: (LitInt <: g) => Fix g -> Int
projArith elem = case fromJust (projF elem) of
  (Lit int) -> int
