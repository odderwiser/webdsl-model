module Templates.Modules.Phases.Syntax where
import Data.Bifunctor (Bifunctor, bimap)

data Databind t a = Databind a -- propably?
    deriving Functor 

instance Bifunctor Databind where
    bimap g f (Databind v) = Databind $ f v

data Validate t a = Validate a String -- tuple of expression to satisfy ? and error to throw
    deriving Functor

instance Bifunctor Validate where
    bimap g f (Validate a s) = Validate (f a) s

data Action t a = Action a
    deriving Functor

instance Bifunctor Action where
    bimap g f (Action a) = Action $ f a