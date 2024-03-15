module Eval.Interface where
import Eval.Syntax 
import Utils.Denote (Env)
import Utils.Free (Free)
import Eval.Effects 
import Utils.Composition (type (<:), type (<) (injV))
import Syntax --(Val (..), unwrap, Address, wrap)
import Arith.Syntax (LitAr)
import Bool.Syntax (LitB)
import Utils.Handler (handle_)

derefEnv :: VName -> Env ->  Address
derefEnv name env = case lookup name env of 
    Just loc -> loc

denote :: forall v eff. (MLState Address v <: eff, LitAr < v, LitB < v, LitN < v)
  => Eval (Env -> Free eff v)
  -> Env -> Free eff v

denote (Var name)              env = do 
    deref (derefEnv name env)

denote (VDecl name k)        env = do
    loc <- ref (injV Null :: v)
    k $ (name, loc) : env 

denote (VValDecl name e typ k) env = do
    v   <- e env
    loc <- ref v
    k $ (name, loc) : env 

denote (VAssign name e typ)    env = do
    v <- e env
    assign (derefEnv name env , v)
    return $ injV Null
    -- this possibly might need to be null instead
    -- in a correct program this will never happen
    -- but this also might lead to leaking values in incorrect program?

