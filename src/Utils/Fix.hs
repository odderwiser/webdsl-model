module Utils.Fix where
import Utils.Composition

data Fix f = In (f (Fix f))

injF :: (f <: g) => f (Fix g) -> Fix g 
injF = In . inj

class (f <: g) => BinaryInject f g op where
    bin ::  op -> f (Fix g) -> f (Fix g) -> Fix g
