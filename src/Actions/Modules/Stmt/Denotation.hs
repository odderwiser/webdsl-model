module Actions.Modules.Stmt.Denotation where
import Utils
import Actions.Modules.Stmt.Syntax
import Syntax (Address)
import Actions.Effects as E
import Actions.Modules.Eval.Denotation (refEnv)
import Actions.Modules.Bool.Syntax as B
import Actions.Modules.Eval.Syntax (VName)
import Data.Maybe (mapMaybe, catMaybes)
import Actions.Modules.Arith.Syntax as A
import Data.List (sort, sortOn)
import Actions.Modules.Col.Syntax (projC)
import Actions.Values
import Templates.Modules.Lift.Syntax

denote :: forall v eff. (Functor eff)
  => Stmt (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)

denote = denoteStmt

denoteStmt (S s1 s2) env = do
  s1' <- s1 env
  s2 env

denoteLoop :: forall v eff. 
  ( Null <: v, [] <: v, LitBool <: v, LitInt <: v
  , Cond <: eff, MLState Address (Fix v) <: eff
  ) => Weaken Loop (FreeEnv eff (Fix v))
    -> FreeEnv eff (Fix v)

denoteLoop (Weaken (ForCol name col stmts filters)) env = do
  col' <- col env
  col'' <- denoteFilters name col' filters env
  executeLoop (projC col'') name env stmts
  -- return $ injF Null

denoteLoop (Weaken (ForArith name e1 e2 stmts)) env = do
  e1' <- e1 env
  e2' <- e2 env
  executeLoop (halfOpenRange e1' e2')
    name env stmts

denoteLoop (Weaken (While e stmts)) env = whileLoop e stmts env

executeLoop col' name env stmts = do
  mapM_ (\id -> do
    loc <- ref id
    env' <- refEnv name loc env
    stmts env') col'
  return $ injF Null

halfOpenRange :: forall g. (LitInt <: g) => Fix g -> Fix g -> [Fix g]
halfOpenRange a b = map (boxV :: Int -> Fix g) [a', (a' + step)..(b' - step) ]
  where step = signum (b' - a')
        a'   = unbox' a
        b'   = unbox' b
        
whileLoop e stmts env = do
  e'      <- e env
  cond (projV e') 
    (executeWhileLoop e stmts env)
    (return $ injF Null)

executeWhileLoop e stmts env = do 
  stmts env
  whileLoop e stmts env

denoteFilters :: (Null <: v, [] <: v, LitBool <: v, LitInt <: v,  MLState Address (Fix v) <: eff)
  => VName -> Fix v -> [Filter (FreeEnv eff (Fix v))]
  -> FreeEnv eff (Fix v)
denoteFilters name col []              env = return col
denoteFilters name col ( f : filters ) env =  do
    col'' <- denoteFilter name (projC col) f env
    denoteFilters name col'' filters env


denoteFilter :: forall eff v. 
  ( Null <: v, [] <: v, LitBool <: v, LitInt <: v
  , MLState Address (Fix v) <: eff
  ) => VName -> [Fix v] -> Filter (FreeEnv eff (Fix v))
     -> FreeEnv eff (Fix v)
denoteFilter name col (Where e) env = applyFunction filter
  unbox'
  name col e env

denoteFilter name col (OrdBy e True) env = applyFunction sortOn
  (unbox' :: Fix v -> Int)
  name col e env

denoteFilter name col (OrdBy e False) env = applyFunction sortOn
  (negate . (unbox' :: Fix v -> Int))
  name col e env

denoteFilter name col (Limit e) env = applyDecrease take
  col e env

denoteFilter name col (Offset e) env = applyDecrease Prelude.drop
  col e env

applyDecrease f col e env = do
  e'            <- e env
  return $ injF 
    $ f (unbox' e') col

applyFunction :: forall a b v f. (MLState Address (Fix v) <: f, [] <: v, Null <: v)
  => (((a, b) -> a) -> [(a, Fix v)] -> [(a, Fix v)])
  -> (Fix v -> a)
  -> VName -> [Fix v] -> FreeEnv f (Fix v) -> FreeEnv f (Fix v)
applyFunction f mapping name col e env  = do
  loc            <- ref (injF Null :: Fix v)
  env'           <- refEnv name loc env
  zipped         <- applyMapping e mapping loc env' col
  return $ injF 
    $ evaluateFilter f zipped

applyMapping e mapping loc env' = mapM (\elem -> do
  assign (loc, elem)
  e' <- e env'
  return (mapping e', elem))

evaluateFilter f = map snd . f fst
