module Str.Denotation where
import Str.Syntax
import Utils.Composition
import Utils.Fix
import Utils.Free
import Utils.Environment

op :: (Functor f, LitStr <: v') 
  => (String -> String -> String) -> Fix v' -> Fix v' 
  -> Free f (Fix v')
op operand e1 e2 = case (projF e1, projF e2) of
  (Just (Lit e1'), Just (Lit e2')) -> return 
    $ injF $ Lit 
    $ operand e1' e2'

denote :: (Functor eff, LitStr <: v) 
  => Str (FreeEnv eff (Fix v)) 
  -> FreeEnv eff (Fix v)
denote (LitS str) env = return $ injF (Lit str)

denote (Add a b) env = do 
  a' <- a env 
  b' <- b env 
  op (++) a' b'