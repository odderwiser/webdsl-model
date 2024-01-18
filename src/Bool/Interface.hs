module Bool.Interface where
import Utils.Denote
import Bool.Syntax
import Effects
import Utils.Composition
import Utils.Free

instance (Cond (LitB e) <: eff, Operation OpB (LitB e) <: eff) => Denote 
    Boolean eff LitB 
    where

  denote :: Boolean (Env -> Free eff (LitB e))
    -> Env 
    -> Free eff (LitB e)
  denote (LitB (Lit bool)) env = return (Lit bool)

  denote (OpB op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'

  denote (If condition left right) env = do
    condition' <- condition env
    cond condition' (left env) (right env)

instance (Cond (LitB e) <: eff, LitB <: v) => Denote 
    IfTE eff v where
  denote :: (LitB <: v, Cond (LitB e) <: eff) =>
    IfTE (Env -> Free eff (v e')) -> Env -> Free eff (v e')
  denote (IfTE condition left right) env = do
    (condition' :: LitB e) <- (condition :: Env -> Free eff (LitB e)) env
    cond condition' (left env) (right env)

-- instance Functor eff =>  Denote Boolean (Cond v + eff) v where  
--   denote :: Boolean (Env -> Free (Cond v + eff) v)
--     -> Env 
--     -> Free (Cond v + eff) v
