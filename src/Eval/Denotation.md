-- module Eval.Denotation where
-- import Eval.Syntax
-- import Utils.Free (Free)
-- import Eval.Effects
-- import Utils.Composition (type (<:), type (<) (injV), type (+), inj)
-- import Syntax --(Val (..), unwrap, Address, wrap)
-- import Utils.Handler (handle_, Handler_)
-- import Eval.Handlers (environment)
-- import Utils.Fix (Fix, injF)
-- import Utils.Environment


-- derefEnv :: (Functor eff) 
--   => VName -> Env eff v -> Free eff Address
-- derefEnv name env = do
--   (loc, env) <- handle_ environment env (deref name)
--   return loc

-- refEnv :: (Functor eff)
--   => VName -> Address 
--   -> Env eff v -> Free eff (Env eff v)
-- refEnv name loc env = do
--   (_, env') <- handle_ environment env (assign (name, loc))
--   return env'


-- -- refEnv newEnv env = do 
-- --     (_ , env') <- handle_ environment env 
-- --         $ mapM assign newEnv
-- --     return env'

-- denote :: forall v eff. ( MLState Address (Fix v) <: eff, Null <: v)
--   => Eval (FreeEnv eff (Fix v))
--   -> FreeEnv eff (Fix v)

-- denote (Var name)            env = do
--   loc         <- derefEnv name env
--   deref loc

-- denote (VDecl name k)        env = do
--   loc   <- ref (injF Null :: Fix v)
--   env'  <- refEnv name loc env 
--   k env'

-- denote (VValDecl name e k) env = do
--   v     <- e env
--   loc   <- ref v
--   env'  <- refEnv name loc env
--   k env'

-- denote (VAssign name e)    env = do
--   v <- e env
--   loc <- derefEnv name env
--   assign (loc, v)
--   return $ injF Null

