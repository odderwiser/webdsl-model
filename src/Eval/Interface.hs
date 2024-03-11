module Eval.Interface where
import Eval.Syntax 
import Utils.Denote (Env)
import Utils.Free (Free)
import Eval.Effects 
import Utils.Composition (type (<:), type (<))
import Syntax (Val (..), unwrap, Address, wrap)
import Arith.Syntax (LitAr)
import Bool.Syntax (LitB)
import Utils.Handler (handle_)
import Eval.Handlers (environment)

derefEnv :: VName -> Env ->  Address
derefEnv name env = case lookup name env of 
    Just loc -> loc

denote :: (MLState Address Val <: eff, LitAr < v, LitB < v)
  => Eval (Env -> Free eff v)
  -> Env -> Free eff v

denote (Var name)           env = do 
    v <- deref (derefEnv name env)
    unwrap v

denote (VDecl name e typ k) env = do
    v   <- e env
    loc <- ref $ wrap v typ
    k $ (name, loc) : env 

denote (VAssign name e typ) env = do
    v <- e env
    assign (derefEnv name env , wrap v typ)
    return v 
    -- this possibly might need to be null instead
    -- in a correct program this will never happen
    -- but this also might lead to leaking values in incorrect program?

