module Actions.Modules.Col.Denotation where

import Actions.Modules.Stmt.Denotation (denoteFilters)
import Actions.Modules.Eval.Denotation (refEnv)
import Actions.Modules.Col.Syntax
import Actions.Arith as A
import Actions.Bool as B

import Utils
import Syntax as S

import Data.Maybe (mapMaybe)
import Actions.Effects (MLState, assign, ref)
import Actions.Values as V

denote :: forall f eff. (Eq (f (Fix f)), LitBool <: f, [] <: f, LitInt <: f, Null <: f,
  MLState Address (Fix f) <: eff)
  => Col (FreeEnv eff (Fix f))
  -> FreeEnv eff (Fix f)
denote (LitC v) env = do
  v' <- mapM (\x -> x env) v
  return $ injF v'

denote (OpIn a b) env = do
  a' <- a env
  b' <- b env
  elemContains a' b'

denote c@(LComp Nothing exp name col filters) env = do
  col' <- listComprehension c env
  return $ injF col'

denote c@(LComp (Just And) exp name col filters) env = do
  col' <- listComprehension c env
  foldList (&&) True col'

denote c@(LComp (Just Or) exp name col filters) env = do
  col' <- listComprehension c env
  foldList (||) False col'

listComprehension :: forall f eff. (Null <: f, LitBool <: f, [] <: f, LitInt <: f,
  MLState Address (Fix f) <: eff)
  => Col (FreeEnv eff (Fix f))
  -> Env eff (Fix f) -> Free eff [Fix f]
listComprehension (LComp e exp name col filters) env = do
  loc    <- ref (V.null :: Fix f)
  env'   <- refEnv name loc env
  col'   <- col env
  col''  <- denoteFilters name col' filters env
  forAll exp env' loc $ projC col''

forAll exp env' loc = mapM (\elem -> do
    assign (loc, elem)
    exp env')

elemContains :: ([] <: g, Functor f, LitBool <: g, Eq (g (Fix g)))
    => Fix g -> Fix g -> Free f (Fix g)
elemContains e1 e2 = case projF e2 of
  (Just (e2' :: [Fix g])) -> return
    $ boxV
    $ elem e1 e2'

foldList :: (Functor eff, [] <: g, LitBool <: g, LitBool <: g)
  => (Bool -> Bool -> Bool) -> Bool -> [Fix g] -> Free eff (Fix g)
foldList op start e = return
    $ boxV
    $ foldr (op . unbox') start e