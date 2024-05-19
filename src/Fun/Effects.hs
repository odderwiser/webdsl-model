module Fun.Effects where
import Utils.Composition
import Utils.Free

data Abort v k = Abort v
  deriving Functor

data MutateEnv env k 
  = DropLocalVars env (env ->  k)
  | LiftObjectEnv env env (env -> k)
  deriving Functor

drop :: MutateEnv env <: f => env -> Free f env
drop env = Op $ inj $ DropLocalVars env Pure

lift :: MutateEnv env <: f => env -> env -> Free f env
lift globalEnv objEnv = Op $ inj $ LiftObjectEnv globalEnv objEnv Pure

abort :: Abort v <: f => v -> Free f a
abort val = Op (inj (Abort val))