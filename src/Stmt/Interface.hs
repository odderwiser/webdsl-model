module Stmt.Interface where
import Utils.Denote (Env)
import Stmt.Syntax
import Utils.Free (Free)

denote :: (Functor eff)
  => Stmt (Env -> Free eff v)
  -> Env -> Free eff v

denote (S s1 s2) env = do
    s1' <- s1 env
    s2 env

