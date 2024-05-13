module Stmt.Denotation where
import Utils.Denote (Env, FreeEnv)
import Stmt.Syntax
import Utils.Free (Free)
import Stmt.Effects as S hiding (filter)
import Utils.Fix
import Utils.Composition
import Eval.Handlers (environment)
import Syntax (Null(Null), Address)
import Eval.Effects
import Eval.Denotation (refEnv)
import qualified Bool.Syntax as B
import Eval.Syntax (VName)
import Data.Maybe (mapMaybe, catMaybes)
import Bool.Syntax (LitBool)
import qualified Arith.Syntax as A
import Arith.Syntax
import Data.List (sort, sortOn)
import Bool.Effects

executeLoop col' name env stmts = case projF col' of
    Just (ids :: [Fix v]) ->
      mapM_ (\id -> do
        loc <- ref id
        env' <- refEnv name loc env
        stmts env') ids

halfOpenRange a b = [a, (a + step)..(b - step) ]
  where step = signum (b - a)

denote :: forall v eff. (Null <: v, [] <: v, LitBool <: v, LitInt <: v, 
  Cond <: eff, MLState Address (Fix v) <: eff)
  => Stmt (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)

denote (S s1 s2) env = do
  s1' <- s1 env
  s2 env

denote (ForCol name col stmts filters) env = do
  col' <- col env
  col'' <- denoteFilters name col' filters env
  case projF col'' of
    Just (ids :: [Fix v]) -> do
      mapM_ (\id -> do
        loc <- ref id
        env' <- refEnv name loc env
        stmts env') ids
  return $ injF Null

denote (ForArith name e1 e2 stmts) env = do
  e1' <- e1 env
  e2' <- e2 env
  case (projF e1', projF e2') of
    (Just (A.Lit e1''), Just (A.Lit e2'')) -> do
      mapM_ (\id -> do
        loc <- ref ((injF $ A.Lit id) :: Fix v)
        env' <- refEnv name loc env
        stmts env') $ halfOpenRange e1'' e2''
  return $ injF Null

denote (While e stmts) env = whileLoop e stmts env

whileLoop e stmts env = do
  e' <- e env
  cond e' (do 
    stmts env
    whileLoop e stmts env)
    (return $ injF Null) 

denoteFilters :: (Null <: v, [] <: v, LitBool <: v, LitInt <: v,  MLState Address (Fix v) <: eff)
  => VName -> Fix v -> [Filter (FreeEnv eff (Fix v))]
  -> FreeEnv eff (Fix v)
denoteFilters name col []              env = return col
denoteFilters name col ( f : filters ) env = case projF col of
  Just (col' :: [Fix v]) -> do
    col'' <- denoteFilter name col' f env
    denoteFilters name col'' filters env


denoteFilter :: (Null <: v, [] <: v, LitBool <: v, LitInt <: v, MLState Address (Fix v) <: eff)
  => VName -> [Fix v] -> Filter (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denoteFilter name col (Where e) env = applyFunction filter
  (\e' -> case projF e' of 
    Just ((B.Lit bool)) -> bool)
  name col e env

denoteFilter name col (OrdBy e True) env = applyFunction sortOn
  (\e' -> case projF e' of 
    Just ((A.Lit num)) -> num)
  name col e env

denoteFilter name col (OrdBy e False) env = applyFunction sortOn
  (\e' -> case projF e' of 
    Just ((A.Lit num)) -> -num)
  name col e env

denoteFilter name col (Limit e) env = applyDecrease take
  col e env

denoteFilter name col (Offset e) env = applyDecrease drop
  col e env

applyDecrease f col e env = do
  e' <- e env
  case projF e' of
    Just (A.Lit e') -> return $ injF $ f e' col
 
applyFunction :: forall a b v f. (MLState Address (Fix v) <: f, [] <: v, Null <: v) 
  => (((a, b) -> a) -> [(a, Fix v)] -> [(a, Fix v)]) 
  -> (Fix v -> a) 
  -> VName -> [Fix v] -> FreeEnv f (Fix v) -> FreeEnv f (Fix v)
applyFunction f mapping name col e env  = do
  loc  <- ref (injF Null :: Fix v)
  env' <- refEnv name loc env
  zipped <- mapM (\elem -> do
    assign (loc, elem)
    e' <- e env'
    return (mapping e', elem)
    ) col
  return $ injF $ map snd $ f fst zipped
