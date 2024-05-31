{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Eta reduce" #-}
{-# LANGUAGE RankNTypes #-}
module Actions.FunTest where
import Syntax
import Utils
import Actions.Bool as B
import Actions.Arith as A
import Actions.Syntax
import Actions.Framework
import Test.HUnit
import TestSyntax

testEq :: Denote m Eff V
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

--------------------------------

testAbort :: Test
testAbort = testEq "two returns"
  (injF $ A.Lit 1)
  abortSyntax

abortSyntax :: Fix Module
abortSyntax = injF
  $ S (injF $ Return $ injA 1 )
  $ injF $ Return $ injA 2


funTests :: Test
funTests = TestList
  [ testAbort
  ]
