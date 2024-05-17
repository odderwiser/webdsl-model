module Program.Effects where
import Entity.Syntax (ScopedType)
import Utils.Composition
import Utils.Environment
import Utils.Free
import Fun.Syntax (FDecl)

data GlobalScope g eff k
    = forall e v. Write EnvType [g (FreeEnv eff v)] (Env eff v) (Env eff v -> k)

data EnvType = Vars | Defs | EDefs

write :: (FDecl <: g, GlobalScope g f' <: f, f ~ GlobalScope g f' + f') 
    => EnvType -> [g (FreeEnv f' v)] -> Env f' v -> Free f (Env f' v)
write ty defs env = Op . inj $ Write ty defs env Pure
