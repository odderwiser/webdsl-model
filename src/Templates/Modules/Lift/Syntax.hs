{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Templates.Modules.Lift.Syntax where
import Data.Bifunctor 
import Utils.Composition
import Utils.Fix

data LiftT s t a = LiftT (s t) 
  deriving Functor

data LiftE s t a = LiftE (s a)
  deriving Functor

instance (Functor s) => Bifunctor (LiftT s) where
  bimap :: (a -> b) -> (c -> d) -> LiftT s a c -> LiftT s b d
  bimap g f (LiftT s) = LiftT $ fmap g s

instance (Functor s) => Bifunctor (LiftE s) where
  bimap g f (LiftE s) = LiftE $ fmap f s 

data (Bifunctor s) => Weaken s e = Weaken (s e e)

instance (Bifunctor s) => Functor (Weaken s) where
  fmap :: (a -> b) -> Weaken s a -> Weaken s b
  fmap  f (Weaken s) = Weaken $ bimap f f s 

