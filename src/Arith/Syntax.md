-- module Arith.Syntax where
-- import Utils.Composition ( type (<:) )
-- import Utils.Fix ( injF, BinaryInject(..), Fix, projF )
-- import Data.Maybe (fromJust)

-- data OpArith = Add | Div | Sub | Mul | Mod

-- data LitInt v = Lit Int
--   deriving (Functor, Eq)

-- instance Show (LitInt a) where
--   show :: LitInt a -> String
--   show (Lit v) = show v 

-- data Arith e = LitAr Int 
--     | OpArith OpArith e e
--     deriving Functor

-- instance (Arith <: g) => BinaryInject Arith g OpArith where
--   bin :: (Arith <: g) 
--     => OpArith 
--     -> g (Fix g) 
--     -> g (Fix g) 
--     -> Arith (Fix g)
--   bin op left right = OpArith op (injF left) (injF right)

-- lit :: Int -> Arith e
-- lit = LitAr  

-- injA :: (Arith <: f) => Int -> Fix f
-- injA =  injF . LitAr

-- projArith :: (LitInt <: g) => Fix g -> Int
-- projArith elem = case fromJust (projF elem) of
--   (Lit int) -> int
