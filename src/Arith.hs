module Arith where
import Infrastructure
import Free
import Effects

-- syntax of Arith
data LitAr = Lit Int 

data OpArith = Add | Div | Sub | Mul | Mod

data Arith e = LitAr LitAr 
    | OpArith OpArith e e  
    deriving Functor

instance (Arith <: g) => BinaryInject Arith g OpArith where
  bin :: (Arith <: g) => OpArith -> Arith ( Fix g) -> Arith (Fix g) -> Fix g
  bin op left right = In $ inj $ OpArith op (In $ inj left) (In $ inj right)

binOp :: (Functor g) => Handler (Operation OpArith LitAr) a g a
binOp = Handler
   { ret = pure
  , hdlr = \case
      (Oper op [Lit v1, Lit v2] k) -> k $ Lit $ calcArith op v1 v2
      }

calcArith :: OpArith -> Int -> Int -> Int
calcArith bop v1 v2 = do
    case bop of
        Add -> (+) v1 v2
        Div -> div v1 v2
        Sub -> (-) v1 v2
        Mul -> (*) v1 v2
        Mod -> mod v1 v2

--does this do what I want it to do? Is this composable
instance Functor eff => Denote Arith (Operation OpArith LitAr + eff) LitAr where
  denote :: Arith (Env -> Free (Operation OpArith LitAr + eff) LitAr) 
    -> Env -> Free (Operation OpArith LitAr + eff) LitAr
  denote (LitAr int) env = return int

  denote (OpArith op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'
