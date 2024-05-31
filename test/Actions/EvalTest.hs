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
  (injF $ A.Lit 5)
  varSyntax
  (Env { varEnv = [("x", 0)]})
  [(0, injF $ A.Lit 3)] 

varSyntax :: Fix Module
varSyntax = injF $
    OpArith Add
        (injVar "x")
        (injA 2)

testVDecl :: Test
testVDecl = testEq
 "vDecl"
  (injF Null)
  vDeclSyntax

vDeclSyntax :: Fix Module
vDeclSyntax = injF $
  VDecl "x" $ injF $
  VAssign "x" (injB True)

testVValDecl :: Test
testVValDecl = testEq
 "vValDecl"
  (injF $ A.Lit 8)
  vValDeclSyntax

vValDeclSyntax :: Fix Module
vValDeclSyntax = injF $
  VValDecl "x" (injA 4) 
    $ injF $ OpArith Mul (injVar "x") (injA 2)

testVAssign :: Test
testVAssign = testEq
 "vAssign"
  (injF Null)
  vAssignSyntax

vAssignSyntax :: Fix Module
vAssignSyntax = injF 
  $ VValDecl "x" (injA 4)
  $ injF $ VAssign "x" (injA 8)

testTwoVarsA :: Test
testTwoVarsA = testEq "two variables"
  (injF $ A.Lit 4)
  twoVarsASyntax

twoVarsASyntax :: Fix Module
twoVarsASyntax = injF 
  $ VValDecl "x" (injA 4)
  $ injF $ VValDecl "y" (injA 3) (injVar "x")

testTwoVarsB :: Test
testTwoVarsB = testEq
 "two variables"
  (injF $ A.Lit 3)
  twoVarsBSyntax

twoVarsBSyntax :: Fix Module
twoVarsBSyntax = injF 
  $ VValDecl "x" (injA 4)
  $ injF $ VValDecl "y" (injA 3)
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
