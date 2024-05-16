module Col.Denotation where
import Utils.Composition
import Utils.Free
import Col.Syntax
import Utils.Fix
import Bool.Syntax as B
import Data.Maybe (mapMaybe)
import Utils.Environment (FreeEnv)

elemContains :: ([] <: g, Functor f, LitBool <: g, Eq (g (Fix g)))
    => Fix g -> Fix g -> Free f (Fix g)
elemContains e1 e2 = case projF e2 of
  (Just (e2' :: [Fix g])) -> return
    $ injF $ B.Lit
    $ elem e1 e2'

foldList :: (Functor eff, [] <: g, LitBool <: g, LitBool <: g) 
  => (Bool -> Bool -> Bool) -> Bool -> Fix g -> Free eff (Fix g)
foldList op start e = case projF e of
  (Just (e' :: [Fix g])) -> return
    $ injF $ B.Lit
    $ foldr (op . (\x -> case projF x of (Just (B.Lit x')) -> x')) start e'

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

denote (UnOp And a) env = do
  a' <- a env
  foldList (&&) True a'

denote (UnOp Or a) env = do
  a' <- a env
  foldList (||) False a'