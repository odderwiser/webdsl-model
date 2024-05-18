module Entity.Effects where
import Utils.Composition
import Utils.Free

data Write e k = Write e k
    deriving Functor

write :: Write e <: f => e -> Free f ()
write value = Op $ inj $ Write value $ Pure ()