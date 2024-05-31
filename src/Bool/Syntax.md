-- module Bool.Syntax where
-- import Utils.Composition ( type (<:) )
-- import Utils.Fix ( injF, BinaryInject(..), Fix, projF )
-- import Data.Maybe (fromJust)
-- import Syntax (Type(Bool))

-- data OpB = Or | And 

-- data LitBool e = Lit Bool
--   deriving (Functor, Eq)

-- instance Show (LitBool a) where
--   show :: LitBool a -> String
--   show (Lit v) = show v 

-- data Boolean e = LitB Bool
--     | OpB OpB e e  
--     | If e e e
--   deriving Functor

-- instance (Boolean <: g) 
--     => BinaryInject Boolean g OpB where
--   bin :: OpB 
--     -> g (Fix g)
--     -> g (Fix g)  
--     -> Boolean (Fix g)
--   bin op left right = 
--     OpB op (injF left) (injF right)

-- lit :: Bool -> Boolean e
-- lit = LitB 

-- injB :: (Boolean <: f) => Bool -> Fix f
-- injB =  injF . LitB

-- -- projBool :: forall f. (LitBool <: f) => Fix f -> Bool
-- projBool elem = case fromJust (projF elem) of
--   (Lit bool) -> bool