module Actions.Modules.Arith.Syntax  where
import Utils.Composition ( type (<:) )
import Utils.Fix ( injF, Fix, projF )
import Data.Maybe (fromJust)
import Data.Aeson (ToJSON)
import GHC.Generics (Generic)
import Actions.Values (Lit (Box), box, boxV)

-- VALUES

type LitInt = Lit Int  

boxI :: (LitInt <: v) => Int -> Fix v
boxI = boxV

-- smart constructor

-- lit :: (LitInt <: v) => Int -> Fix v
-- lit = injF . Box

--- SYNTAX

data OpArith = Add | Div | Sub | Mul | Mod

data Arith e = LitAr Int 
    | OpArith OpArith e e
    deriving Functor

-- smart constructors

bin :: (Arith <: g) 
  => OpArith -> Fix g -> Fix g -> Fix g
bin op left right = injF $ OpArith op left right

add :: (Arith <: f) => Fix f -> Fix f -> Fix f
add = bin Add 

subtract :: (Arith <: f) => Fix f -> Fix f -> Fix f
subtract = bin Sub

multiply :: (Arith <: f) => Fix f -> Fix f -> Fix f
multiply = bin Mul

divide :: (Arith <: f) => Fix f -> Fix f -> Fix f
divide = bin Div

modulo :: (Arith <: f) => Fix f -> Fix f -> Fix f
modulo = bin Mod

int :: (Arith <: f) => Int -> Fix f
int =  injF . LitAr

-- projArith :: (LitInt <: g) => Fix g -> Int
-- projArith elem = case fromJust (projF elem) of
--   (Lit int) -> int
