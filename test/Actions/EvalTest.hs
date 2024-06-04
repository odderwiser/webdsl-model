module Actions.EvalTest where
import Actions.Framework
import Syntax hiding (unwrap)
import Utils
import Actions.Bool as B
import Actions.Arith as A
import Test.HUnit
import TestSyntax
import Actions.Syntax

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

testEqEnv :: String -> Out -> Fix Module 
  -> Env Eff V -> [(Address, V)] -> Test
testEqEnv id res syntax env heap = TestCase 
  $ assertEqual id res 
  $ run (foldD syntax) env heap

testVar :: Test
testVar = testEqEnv
 "var with env"
  (A.lit 5)
  varSyntax
  (Env { varEnv = [("x", 0)]})
  [(0, injF $ A.Lit 3)] 

varSyntax :: Fix Module
varSyntax = A.bin Add
        (injVar "x")
        (injA 2)

testVDecl :: Test
testVDecl = testEq
 "vDecl"
  (injF Null)
  vDeclSyntax

vDeclSyntax :: Fix Module
vDeclSyntax = varDecl "x" $ varAssign "x" true

testVValDecl :: Test
testVValDecl = testEq
 "vValDecl"
  (A.lit 8)
  vValDeclSyntax

vValDeclSyntax :: Fix Module
vValDeclSyntax = varInit "x" (injA 4) 
    $ A.bin Mul (injVar "x") (injA 2)

testVAssign :: Test
testVAssign = testEq
 "vAssign"
  (injF Null)
  vAssignSyntax

vAssignSyntax :: Fix Module
vAssignSyntax = varInit "x" (injA 4)
  $ varAssign "x" (injA 8)

testTwoVarsA :: Test
testTwoVarsA = testEq "two variables"
  (A.lit 4)
  twoVarsASyntax

twoVarsASyntax :: Fix Module
twoVarsASyntax = varInit "x" (injA 4)
  $ varInit "y" (injA 3) (injVar "x")

testTwoVarsB :: Test
testTwoVarsB = testEq
 "two variables"
  (A.lit 3)
  twoVarsBSyntax

twoVarsBSyntax :: Fix Module
twoVarsBSyntax = varInit "x" (injA 4)
  $ varInit "y" (injA 3)
  $ injVar "y"


evalTests :: Test
evalTests = TestList 
  [ testVar
  , testVDecl
  , testVValDecl
  , testVAssign
  , testTwoVarsA
  , testTwoVarsB
  ]
