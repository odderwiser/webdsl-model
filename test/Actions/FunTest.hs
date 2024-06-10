{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RankNTypes #-}
module Actions.FunTest where
import Utils
import Actions.Bool as B
import Actions.Arith as A
import Actions.Syntax
import Actions.Framework
import Test.HUnit
import Definitions.Program.Syntax
import Definitions.Fun.Syntax
import Definitions.Fun.Framework
import Definitions.Program.Denotation (foldProgram)

testEq :: Denote m Eff V
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldProgram syntax

--------------------------------


testAbort :: Test
testAbort = testEq "two returns"
  (A.lit 1)
  abortSyntax

abortSyntax :: Fix Module
abortSyntax = cons (return' $ int 1)
  $ return' $ int 2

testfCall :: Test
testfCall = testEqProgram "simple function call"
  (A.lit 7)
  fCallSyntax

fCallSyntax :: Program (FDecl (Fix Module)) (Fix Module)
fCallSyntax = Fragment [FDecl "addThree" ["x"]
    (A.add (var "x") (int 3))]
  $ funCall "addThree" [int 4]

funTests :: Test
funTests = TestList
  [ testAbort
  , testfCall
  ]
