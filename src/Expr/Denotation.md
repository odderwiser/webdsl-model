-- module Expr.Denotation where
-- import Expr.Syntax
-- import Arith.Syntax as A
-- import Bool.Syntax as B
-- import Utils.Composition (type (<:), type (<) (injV), projV)
-- import Utils.Free (Free)
-- import qualified Arith.Denotation as A
-- import qualified Bool.Denotation as B
-- import Syntax (Type (..))
-- import qualified Arith.Syntax as A
-- import Foreign (fromBool)
-- import Utils.Fix
-- import Utils.Environment

-- op :: (Functor f, LitBool <: v)
--   => (a -> a -> Bool) -> a -> a -> Free f (Fix v)
-- op operand e1 e2 = return
--   $ injF $ B.Lit
--   $ operand e1 e2

-- opCmp :: (fix ~ Fix v, Functor f, Num a, LitInt <: v, LitBool <: v)
--   => (a -> a -> Bool) -> fix -> fix
--   -> Free f fix
-- opCmp operand exp1 exp2 = op operand (toNum exp1) (toNum exp2)  

-- toNum :: (Num a, LitInt <: g) => Fix g -> a
-- toNum = fromIntegral . projArith
    
    

-- denote :: (fix ~ Fix v,  Eq (v (Fix v)), 
--   Functor eff, LitInt <: v, LitBool <: v)
--   => Expr (Env eff fix -> Free eff fix)
--   -> Env eff fix -> Free eff fix

-- denote (OpCmp Eq e1 e2) env = do
--   e1' <- e1 env
--   e2' <- e2 env
--   op (==) e1' e2'

-- denote (OpCmp Neq e1 e2) env = do
--   e1' <- e1 env
--   e2' <- e2 env
--   op (/=) e1' e2'

-- denote (OpCmp Lt e1 e2) env = do
--   e1' <- e1 env
--   e2' <- e2 env
--   opCmp (<) e1' e2'

-- denote (OpCmp Lte e1 e2) env = do
--   e1' <- e1 env
--   e2' <- e2 env
--   opCmp (<=) e1' e2'

-- denote (OpCmp Gt e1 e2) env = do
--   e1' <- e1 env
--   e2' <- e2 env
--   opCmp (>) e1' e2'

-- denote (OpCmp Gte e1 e2) env = do
--   e1' <- e1 env
--   e2' <- e2 env
--   opCmp (>=) e1' e2'
