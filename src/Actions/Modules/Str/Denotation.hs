module Actions.Modules.Str.Denotation where
import Actions.Modules.Str.Syntax hiding (length)
import Utils
import Actions.Values
import Actions.Arith (LitInt)

op :: (Functor f, LitStr <: v) 
  => (String -> String -> String) -> Maybe String -> Maybe String 
  -> Free f (Fix v)
op operand (Just e1) (Just e2) = v $ operand e1 e2

unOp :: (Functor f, LitStr <: v, Lit a <: v) 
  => (String -> a) -> Maybe String
  -> Free f (Fix v)
unOp operand (Just e) = v $ operand e

denote :: (Functor eff, LitStr <: v, LitInt <: v) 
  => Str (FreeEnv eff (Fix v)) 
  -> FreeEnv eff (Fix v)
denote (LitS str) env = v str

denote (Add a b) env = do 
  a' <- a env 
  b' <- b env 
  op (++) (projV a') (projV b')

denote (Length str) env = do
  str' <- str env
  unOp length (projV str')