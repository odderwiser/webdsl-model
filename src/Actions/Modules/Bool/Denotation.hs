module Actions.Modules.Bool.Denotation where

import Actions.Modules.Bool.Syntax
import Actions.Effects (Cond, cond)

import Utils
import Actions.Values


op :: (Functor f, LitBool <: v')
  => (Bool -> Bool -> Bool) -> Fix v' -> Fix v' -> Free f (Fix v')
op operand e1 e2 = case (unbox e1, unbox e2) of
  (e1', e2') -> return
    $ box
    $ operand e1' e2'

denote :: (Cond <: eff, LitBool <: v)
  => Boolean (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (LitB bool) env = return $ box bool

denote (OpB Or a b) env = do
  a' <- a env
  b' <- b env
  op (||) a' b'

denote (OpB And a b) env = do
  a' <- a env
  b' <- b env
  op (&&) a' b'

denote (If c a b) env = do
  c' <- c env
  cond c' (a env) (b env)
