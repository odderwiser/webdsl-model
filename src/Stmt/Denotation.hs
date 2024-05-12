module Stmt.Denotation where
import Utils.Denote (Env, FreeEnv)
import Stmt.Syntax
import Utils.Free (Free)
import Stmt.Effects as S
import Utils.Fix
import Utils.Composition
import Eval.Handlers (environment)

denote :: (FilterEff v <: eff)
  => Stmt (Env eff v -> Free eff v)
  -> Env eff v -> Free eff v

denote (S s1 s2) env = do
  s1' <- s1 env
  s2 env

-- denote (ForC name col stmts filters) env = do
--   col' <- col env 
--   filters' <- mapM ()
--   col'' <- mapM (\ftr -> filter)

denoteFilter :: (Functor eff)
  => Filter (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denoteFilter (Where e) env = do
  e' <- e env
  return $ Filter $ injF e' 
