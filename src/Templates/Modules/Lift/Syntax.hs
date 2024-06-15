{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
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

data (Bifunctor s) => Weaken s e = Weaken (s e e)

instance (Bifunctor s) => Functor (Weaken s) where
  fmap :: (a -> b) -> Weaken s a -> Weaken s b
  fmap  f (Weaken s) = Weaken $ bimap f f s 
