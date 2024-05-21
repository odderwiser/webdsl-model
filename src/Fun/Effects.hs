module Fun.Effects where
import Utils.Composition
import Utils.Free

data Abort v k = Abort v
  deriving Functor

data DropEnv env k 
  = DropLocalVars env (env ->  k)
  | DropAttributes env (env -> k)
  --objects only
  deriving Functor

drop :: DropEnv env <: f => env -> Free f env
drop env = Op $ inj $ DropLocalVars env Pure

abort :: Abort v <: f => v -> Free f a
abort val = Op (inj (Abort val))