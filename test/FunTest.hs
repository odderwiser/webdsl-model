module FunTest where

import Eval.Effects
import Syntax
import Bool.Effects 
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
import Fun.Interface as F
import Stmt.Interface as S
import Test.HUnit
import TestSyntax
import Utils.Fix
import Eval.Syntax
import Fun.Syntax
import Fun.Effects
import Fun.Handlers (funReturn, defs)
import Stmt.Syntax as S


type Eff = MLState Address V + Cond + Abort V + End
type V =  Bool \/ Int \/ Null
type Module = Arith + Boolean + Eval + Fun + Program + Stmt
type Out = Maybe (Either Bool Int)

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

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = Ex.denote

instance Denote Eval Eff V where
  denote = Ev.denote

instance Denote Fun Eff V where
  denote = F.denote

instance Denote Program Eff V where
  denote = F.denoteProgram

instance Denote Stmt Eff V where
  denote = S.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

--------------------------------

testAbort :: Test
testAbort = testEq "two returns"
  (Just $ Right 1)
  abortSyntax

abortSyntax :: Fix Module
abortSyntax = injF $ S 
  (injF $ Return (injF $ A.lit 1)) 
  (injF $ Return (injF $ A.lit 2))

funTests :: Test
funTests = TestList [
    testAbort
    ]
