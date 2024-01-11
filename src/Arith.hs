module Arith where
import Infrastructure
import Free

-- syntax of Arith
data LitAr = Lit Int 

data OpArith = Add | Div | Sub | Mul | Mod

data Arith e = LitAr LitAr 
    | OpArith OpArith e e  
    deriving Functor

-- effects

data Operation p v k
  = Oper p [v] (v -> k)
  deriving Functor

aryOp :: Operation p v <: f => p -> [v] -> Free f v
aryOp param e = Op $ inj $ Oper param e Pure

--does this do what I want it to do? Is this composable
instance Functor eff => Denote Arith (Operation OpArith LitAr + eff) LitAr where
  denote :: Arith (env -> Free (Operation OpArith LitAr + eff) LitAr) ->env ->  Free (Operation OpArith LitAr + eff) LitAr
  denote (LitAr int) env = return int

  denote (OpArith op a b) env = do 
    a' <- a env 
    b' <- b env 
    aryOp op [a', b']
