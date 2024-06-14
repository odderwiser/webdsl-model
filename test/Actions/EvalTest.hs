module Actions.EvalTest where
import Actions.Framework
import Syntax as S hiding (unwrap)
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
  (boxI 5)
  varSyntax
  (Env { varEnv = [("x", 0)]})
  [(0, boxI 3)] 

varSyntax :: Fix Module
varSyntax = A.add (var "x") (int 2)

testVDecl :: Test
testVDecl = testEq
 "vDecl"
  S.null
  vDeclSyntax

vDeclSyntax :: Fix Module
vDeclSyntax = varDecl "x" $ varAssign "x" true

testVValDecl :: Test
testVValDecl = testEq
 "vValDecl"
  (boxI 8)
  vValDeclSyntax

vValDeclSyntax :: Fix Module
vValDeclSyntax = varInit "x" (int 4) 
    $ multiply (var "x") (int 2)

testVAssign :: Test
testVAssign = testEq
 "vAssign"
  (injF Null)
  vAssignSyntax

vAssignSyntax :: Fix Module
vAssignSyntax = varInit "x" (int 4)
  $ varAssign "x" (int 8)

testTwoVarsA :: Test
testTwoVarsA = testEq "two variables"
  (boxI 4)
  twoVarsASyntax

twoVarsASyntax :: Fix Module
twoVarsASyntax = varInit "x" (int 4)
  $ varInit "y" (int 3) (var "x")

testTwoVarsB :: Test
testTwoVarsB = testEq
 "two variables"
  (boxI 3)
  twoVarsBSyntax

twoVarsBSyntax :: Fix Module
twoVarsBSyntax = varInit "x" (int 4)
  $ varInit "y" (int 3)
  $ var "y"


evalTests :: Test
evalTests = TestList 
  [ testVar
  , testVDecl
  , testVValDecl
  , testVAssign
  , testTwoVarsA
  , testTwoVarsB
  ]
