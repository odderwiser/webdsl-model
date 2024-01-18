module Bool where
import Effects

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
  denote :: Boolean (Env -> Free (Operation OpB LitB + eff) LitB) -> Env -> Free (Operation OpB LitB + eff) LitB
  denote (LitB bool) env = return bool

  denote (OpB op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'

instance Functor eff =>  Denote Boolean (Cond v + eff) v where  
  denote :: Functor eff =>
    Boolean (Env -> Free (Cond v + eff) v)
    -> Env -> Free (Cond v + eff) v
  denote (If condition left right) env = do
    condition' <- condition env
    cond condition' (left env) (right env)

