module Bool.Interface where
import Utils.Denote
import Bool.Syntax
import Effects
import Utils.Composition
import Utils.Free

instance (Cond <: eff, Operation OpB LitB <: eff, LitB < v) => Denote 
    Boolean eff v 
    where
  denote :: (Cond <: eff, Operation OpB LitB <: eff, LitB < v) => 
    Boolean (Env -> Free eff v) 
    -> Env -> Free eff v
  denote (LitB bool) env = return $ injV bool

-- co tu się dzieje? Czegoś mi brakuje??
  denote (OpB op a b) env = do 
    a' <- a env 
    b' <- b env 
    case (projV a' :: Maybe LitB, projV b' :: Maybe LitB) of
      (Just a'', Just b'') -> binaryOp op a'' b''

  denote (If c a b) env = do 
    c' <- c env
    cond c' (a env) (b env)


-- instance Functor eff =>  (Denote Boolean (Cond + eff) v, LitB < v) where  
--   denote :: Boolean (Env -> Free (Cond  + eff) v)
--     -> Env 
--     -> Free (Cond  + eff) v
--   denote (If condition left right) env = do
--     condition' <- condition env
--     cond condition' (left env) (right env)
