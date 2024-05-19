module Col.Denotation where
import Utils.Composition
import Utils.Free
import Col.Syntax
import Utils.Fix
import Bool.Syntax as B
import Data.Maybe (mapMaybe)
import Utils.Environment (FreeEnv, Env)
import Stmt.Denotation (denoteFilters)
import Eval.Effects (MLState, ref, assign)
import Syntax as S
import Eval.Denotation (refEnv)
import Arith.Syntax (LitInt)

elemContains :: ([] <: g, Functor f, LitBool <: g, Eq (g (Fix g)))
    => Fix g -> Fix g -> Free f (Fix g)
elemContains e1 e2 = case projF e2 of
  (Just (e2' :: [Fix g])) -> return
    $ injF $ B.Lit
    $ elem e1 e2'

foldList :: (Functor eff, [] <: g, LitBool <: g, LitBool <: g)
  => (Bool -> Bool -> Bool) -> Bool -> Fix g -> Free eff (Fix g)
foldList op start e = return
    $ injF $ B.Lit
    $ foldr (op . projBool) start $ projC e

denote :: forall f eff. (Eq (f (Fix f)), LitBool <: f, [] <: f, LitInt <: f, Null <: f,
  MLState Address (Fix f) <: eff)
  => Col (FreeEnv eff (Fix f))
  -> FreeEnv eff (Fix f)
denote (LitC v) env = do
  v' <- mapM (\x -> x env) v
  return $ injF v'

denote (OpIn a b) env = do
  a' <- a env
  b' <- b env
  elemContains a' b'

denote (LComp exp name col filters) env = do
  loc    <- ref (S.null :: Fix f)
  env'   <- refEnv name loc env
  col'   <- col env
  col''  <- denoteFilters name col' filters env
  col''' <- forAll exp env' loc $ projC col''
  return $ injF col'''

denote (UnOp And a) env = do
  a' <- a env
  foldList (&&) True a'

denote (UnOp Or a) env = do
  a' <- a env
  foldList (||) False a'

forAll exp env' loc = mapM (\elem -> do
    assign (loc, elem)
    exp env')