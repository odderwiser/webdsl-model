module Bool.Syntax where
import Utils.Composition
import Utils.Fix
import Utils.Denote (Env)
import Utils.Free

data LitB e = Lit Bool 
    deriving Functor

data OpB = Or | And 

data Boolean e = LitB (LitB e) 
    | OpB OpB e e  
    | If e e e
    deriving Functor

data IfTE eff e' e = IfTE (Env -> Free eff (LitB e')) e e
    deriving Functor

instance (Boolean <: g) 
    => BinaryInject Boolean g OpB where
  bin :: OpB 
    -> g (Fix g)
    -> g (Fix g)  
    -> Boolean (Fix g)
  bin op left right = 
    OpB op (injF left) (injF right)


ifTE :: (Boolean <: g) 
    => Boolean (Fix g) 
    -> Boolean (Fix g) 
    -> Boolean (Fix g) 
    -> Fix g
ifTE cond th el = injF $ If (injF cond) (injF th) (injF el)