module Bool.Interface where
import Utils.Denote
import Bool.Syntax
import Effects
import Utils.Composition
import Utils.Free

instance Functor eff => Denote 
    Boolean (Operation OpB LitB + eff) LitB 
    where
  denote :: Boolean (Env -> Free (Operation OpB LitB + eff) LitB) 
    -> Env -> Free (Operation OpB LitB + eff) LitB
  denote (LitB bool) env = return bool

  denote (OpB op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'

instance Functor eff =>  Denote Boolean (Cond v + eff) v where  
  denote :: Boolean (Env -> Free (Cond v + eff) v)
    -> Env 
    -> Free (Cond v + eff) v
  denote (If condition left right) env = do
    condition' <- condition env
    cond condition' (left env) (right env)
