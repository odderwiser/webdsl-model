module Program.Effects where
import Entity.Syntax 
import Utils.Composition
import Utils.Environment
import Utils.Free

data GlobalScope g eff v k
    = Write EnvType [g (FreeEnv eff v)] (Env eff v) (Env eff v -> k)
    deriving Functor


data EnvType = Vars | Defs | Entities

write :: (GlobalScope g eff v <: f, env ~ Env eff v)
    => EnvType -> [g (FreeEnv eff v)] -> env -> Free f env
write ty defs env = Op . inj $ Write ty defs env Pure
