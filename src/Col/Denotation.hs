module Col.Denotation where
import Utils.Composition
import Utils.Denote
import Utils.Free
import Col.Syntax
import Syntax
import Utils.Fix
import Bool.Syntax as B
import Arith.Syntax

elemContains :: ([] <: g, Functor f, LitBool <: g, Eq (g (Fix g)))
    => Fix g -> Fix g -> Free f (Fix g)
elemContains e1 e2 = case projF e2 of
  (Just (e2' :: [Fix g])) -> return
    $ injF $ B.Lit
    $ elem e1 e2'

denote :: (Eq (f (Fix f)), Functor eff, LitBool <: f, [] <: f)
  => Col (FreeEnv eff (Fix f))
  -> FreeEnv eff (Fix f)
denote (LitC v) env = do
  v' <- mapM (\x -> x env) v
  return $ injF v'

denote (OpIn a b) env = do
  a' <- a env
  b' <- b env
  elemContains a' b'
