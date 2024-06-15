module Actions.ExprTest where
import Utils
import Actions.Arith as A 
import Actions.Bool as B 
import Actions.Syntax
import Test.HUnit 
import TestSyntax 
import Syntax (Type(..))
import Actions.Framework
import Actions.Values

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

-------- regression tests ---------

testIf :: Test
testIf = testEq 
  "ifSimple" 
  (boxV True)
  ifSimple
    

testIfComplicated :: Test
testIfComplicated = testEq
  "ifComplicated"
  (boxV True)
  ifComplicated

testIfAB :: Test
testIfAB = testEq "ifAB"
  (boxI 2)
  ifSyntax

testIfComp :: Test
testIfComp = testEq
 "ifComparison"
  (boxI 1)
  ifComparison

----------- new feature tests ------------

testEqu :: Test
testEqu = testEq "eq"
  (boxV True)
  eqSyntax

testCmp :: Test
testCmp = testEq
 "comparison"
  (boxV True)
  cmpSyntax

exprTests :: Test
exprTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEqu,
    testCmp
    ]
