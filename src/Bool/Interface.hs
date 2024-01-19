module Bool.Interface where
import Utils.Denote
import Bool.Syntax
import Effects
import Utils.Composition
import Utils.Free

instance (Cond (LitB e1) <: eff, Operation OpB (LitB e1) <: eff) => Denote
    Boolean eff LitB where
  denote :: (Cond (LitB e1) <: eff, Operation OpB (LitB e1) <: eff) =>
    Boolean (Env -> Free eff (LitB e)) -> Env -> Free eff (LitB e)
  -- denote :: (Cond (LitB e) <: eff, Operation OpB (LitB e) <: eff) =>
  --   Boolean (Env -> Free eff (LitB e)) -> Env -> Free eff (LitB e)


  denote (LitB (Lit bool)) env = return (Lit bool)

  denote (OpB op a b) env = do
    a' <- a env
    b' <- b env
    binaryOp op (coerce a' :: LitB e1) (coerce b')

  denote (If condition left right) env = do
    condition' <- condition env
    cond 
      (coerce condition' :: LitB e1) 
      (coerceFree $ left env) 
      (coerceFree $ right env)

instance (Literal v, Cond (LitB e) <: eff, LitB <: v) => Denote
    (IfTE eff e) eff v where
  denote :: (LitB <: v, Cond (LitB e) <: eff) =>
    (IfTE eff e) (Env -> Free eff (v e')) -> Env -> Free eff (v e')
  denote (IfTE condition left right) env = do
    condition' <- condition env
    cond 
      (coerce condition' :: LitB e) 
      (coerceFree $ left env) 
      (coerceFree $ right env)

-- instance Functor eff =>  Denote Boolean (Cond v + eff) v where  
--   denote :: Boolean (Env -> Free (Cond v + eff) v)
--     -> Env 
--     -> Free (Cond v + eff) v
