module Fun.Effects where
import Utils.Composition
import Utils.Free

data Abort v k = Abort v
  deriving Functor

data Drop env k = DropEnvironment env (env ->  k)
  deriving Functor

drop :: Drop env <: f => env -> Free f env
drop env = Op $ inj $ DropEnvironment env Pure

abort :: Abort v <: f => v -> Free f a
abort val = Op (inj (Abort val))