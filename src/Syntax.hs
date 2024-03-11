module Syntax where
import Bool.Syntax (LitB)
import Arith.Syntax (LitAr)
import Utils.Composition (type (<), injV, projV)
import Utils.Free (Free)

-- for now, they will be only included where they are necessary
type Address = Int

data Type  = Int | Bool 
data Val
    = VBool LitB
    | VInt LitAr

unwrap :: (Functor eff, LitAr < v, LitB < v)
    => Val -> Free eff v
unwrap v = return $ case v of
        (VInt  v') -> injV v'
        (VBool v') -> injV v'

wrap :: (LitAr < v, LitB < v)
    => v -> Type -> Val
wrap val Int = case projV val of
    Just v -> VInt v
wrap val Bool = case projV val of
    Just v -> VBool v