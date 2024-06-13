module Templates.Modules.Lift.Syntax where
import Utils (BiFix)
import Actions.Syntax
import Utils
import Data.Bifunctor 

data LiftT s e f = LiftT (s f) 
  deriving Functor

instance (Functor s) => Bifunctor (LiftT s) where
  bimap :: (a -> b) -> (c -> d) -> LiftT s a c -> LiftT s b d
  bimap f g (LiftT s) = LiftT $ fmap g s 

consT :: (LiftT Stmt <:: f) => BiFix f e -> BiFix f e -> BiFix f e
consT s1 s2 = injBf $ LiftT (S s1 s2)