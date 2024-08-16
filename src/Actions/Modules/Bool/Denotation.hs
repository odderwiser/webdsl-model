module Actions.Modules.Bool.Denotation where

import Actions.Modules.Bool.Syntax
import Actions.Effects (Cond, cond)

import Utils
import Actions.Values


op :: (Functor f, Lit Bool <: v)
  => (Bool -> Bool -> Bool) -> Maybe Bool -> Maybe Bool -> Free f (Fix v)
op operand (Just e1) (Just e2) = v $ operand e1 e2

denote :: (Cond <: eff, Lit Bool <: v)
  => Boolean (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (LitB bool) env = return $ boxV bool

denote (OpB Or a b) env = do
  a' <- a env
  b' <- b env
  op (||) (projV'' a') (projV'' b')

denote (OpB And a b) env = do
  a' <- a env
  b' <- b env
  op (&&) (projV'' a') (projV'' b')

denote (If c a b) env = do
  c' <- c env
  cond (projV'' c') (a env) (b env)
