module FunTest where

import Eval.Effects
import Syntax hiding (unwrap)
import Utils.Composition
import Bool.Effects (Cond)
import Utils.Composition
import Bool.Syntax as B
import Arith.Syntax as A
import Expr.Syntax
import Utils.Denote
import Utils.Free
import Eval.Handlers
import Utils.Handler
import Bool.Handlers
import Bool.Interface as B
import Arith.Interface as A
import Expr.Interface as Ex
import Eval.Interface as Ev
import Test.HUnit
import TestSyntax
import Utils.Fix
import Eval.Syntax
import Fun.Syntax
import Fun.Effects
import Fun.Handlers (funReturn, defs)


type Eff = MLState Address V + Cond + Abort V + End
type V =  Bool \/ Int \/ Null
type Module = Arith + Boolean + Expr + Eval + Fun + Program

run :: FreeEnv Eff V
  -> Maybe (Either Bool Int)
run e = case unwrap
    $ handle funReturn
    $ handle condition
    $ flipHandle_ handle_ heap' (makeEnv [])
    $ e $ Env []
  of
    (Left val)           -> Just $ Left val
    (Right (Left val))   -> Just $ Right val
    (Right (Right _))    -> Nothing



