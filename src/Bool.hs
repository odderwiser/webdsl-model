module Bool where
import Free 
import Effects
import Infrastructure

data LitB = Lit Bool 

data OpB = Or | And 

data Boolean e = LitB LitB 
    | OpB OpB e e  
    | If e e e
    deriving Functor

binOp :: (Functor g) => Handler (Operation OpB LitB) a g a
binOp = Handler
   { ret = pure
  , hdlr = \case
      (Oper op [Lit v1, Lit v2] k) -> k $ Lit $ calcBool op v1 v2
      }

calcBool :: OpB -> Bool -> Bool -> Bool
calcBool bop v1 v2 = do
    case bop of
        And -> (&&) v1 v2
        Or -> (||) v1 v2


--does this do what I want it to do? Is this composable
instance Functor eff => Denote Boolean (Operation OpB LitB + eff) LitB where
  denote :: Boolean (Env -> Free (Operation OpB LitB + eff) LitB) -> Env ->  Free (Operation OpB LitB + eff) LitB
  denote (LitB bool) env = return bool

  denote (OpB op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'