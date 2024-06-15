module Actions.Modules.Str.Denotation where
import Actions.Modules.Str.Syntax
import Utils
import Actions.Values

op :: (Functor f, LitStr <: v') 
  => (String -> String -> String) -> Fix v' -> Fix v' 
  -> Free f (Fix v')
op operand e1 e2 = case (unbox' e1, unbox' e2) of
  (e1', e2') -> return 
    $ boxV
    $ operand e1' e2'

denote :: (Functor eff, LitStr <: v) 
  => Str (FreeEnv eff (Fix v)) 
  -> FreeEnv eff (Fix v)
denote (LitS str) env = return $ boxV str

denote (Add a b) env = do 
  a' <- a env 
  b' <- b env 
  op (++) a' b'