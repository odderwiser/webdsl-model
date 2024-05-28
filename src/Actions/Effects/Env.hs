module Actions.Effects.Env where

data DropEnv env k 
  = DropLocalVars env (env ->  k)
  | DropAttributes env (env -> k)
  --objects only
  deriving Functor

drop :: DropEnv env <: f => env -> Free f env
drop env = Op $ inj $ DropLocalVars env Pure