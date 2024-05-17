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
import Bool.Denotation as B
import Arith.Denotation as A
import Expr.Denotation as Ex
import Eval.Denotation as Ev
import Fun.Denotation as F
import Stmt.Denotation as S
import Test.HUnit
import TestSyntax
import Utils.Fix
import Eval.Syntax
import Fun.Syntax
import Fun.Effects
import Fun.Handlers (funReturn, defs)
import Stmt.Syntax as S
import Utils.Environment
import Program.Syntax
import Program.Denotation (foldProgram)


type Eff = MLState Address V + Cond + Abort V + End
type V =  Fix (LitBool + LitInt + Null + [])
type Module = Arith + Boolean + Eval VName + Fun + Stmt
type Out = Maybe (Either Bool Int)
type Envs = FDecl 

runProgram (Fragment defs exp) = case 
  refDefs defs Env { varEnv = [], Utils.Environment.defs = []} of
    (Pure env) -> run exp env

runExp e = run e Env { varEnv = []}

run :: FreeEnv Eff V -> Env Eff V
  -> Maybe (Either Bool Int)
run e env = case unwrap
    $ handle funReturn
    $ handle condition
    $ handle_ heap' (makeEnv [])
    $ e env
  of
    In (L (B.Lit val))     -> Just $ Left val
    In (R (L (A.Lit val))) -> Just $ Right val
    In (R (R _))           -> Nothing

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = Ex.denote

instance Denote (Eval VName) Eff V where
  denote = Ev.denote

instance Denote Fun Eff V where
  denote = F.denote

-- instance Denote Program Eff V where
--   denote = F.denoteProgram

instance Denote Stmt Eff V where
  denote = S.denote

testEq :: Denote m Eff V
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

-- testEqProgram :: Denote m Eff V
--   => String -> Out -> Fix m -> Test
testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldProgram syntax

--------------------------------

testAbort :: Test
testAbort = testEq "two returns"
  (Just $ Right 1)
  abortSyntax

abortSyntax :: Fix Module
abortSyntax = injF
  $ S (injF $ Return $ injA 1)
  $ injF $ Return $ injA 2

testfCall :: Test
testfCall = testEqProgram "simple function call"
  (Just $ Right 7)
  fCallSyntax

fCallSyntax :: Program (FDecl (Fix Module)) (Fix Module)
fCallSyntax = Fragment [FDecl "addThree" ["x"]
    (injF $ OpArith Add (injVar "x") (injA 3))]
  $ injF $ FCall "addThree" [injA 4]

funTests :: Test
funTests = TestList
  [ testAbort
  , testfCall
  ]
