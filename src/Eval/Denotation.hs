module Eval.Denotation where
import Eval.Syntax
import Utils.Denote (Env, FreeEnv)
import Utils.Free (Free)
import Eval.Effects
import Utils.Composition (type (<:), type (<) (injV), type (+), inj)
import Syntax --(Val (..), unwrap, Address, wrap)
import Utils.Handler (handle_, Handler_)
import Eval.Handlers (environment)
import Utils.Fix (Fix, injF)


derefEnv :: (Functor eff) 
    => VName -> Env eff v -> Free eff (Address, Env eff v)
derefEnv name env = handle_ environment env (deref name)
refEnv :: (Functor eff)
    => VName -> Address 
    -> Env eff v -> Free eff ((), Env eff v)
refEnv name loc env = handle_ environment env (assign (name, loc))

denote :: forall v eff. ( MLState Address (Fix v) <: eff, Null <: v)
  => Eval (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)

denote (Var [name])            env = do
    (loc, _) <- derefEnv name env
    deref loc

denote (VDecl [name] k)        env = do
    loc <- ref (injF Null :: Fix v)
    (_, env') <- refEnv name loc env 
    k env'

denote (VValDecl [name] e typ k) env = do
    v   <- e env
    loc <- ref v
    (_, env') <- refEnv name loc env
    k env'

denote (VAssign [name] e typ)    env = do
    v <- e env
    (loc, _) <- derefEnv name env
    assign (loc, v)
    return $ injF Null

