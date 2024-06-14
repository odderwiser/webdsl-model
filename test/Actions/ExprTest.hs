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
  (box True)
  ifSimple
    

testIfComplicated :: Test
testIfComplicated = testEq
  "ifComplicated"
  (box True)
  ifComplicated

testIfAB :: Test
testIfAB = testEq "ifAB"
  (box (2 :: Int))
  ifSyntax

testIfComp :: Test
testIfComp = testEq
 "ifComparison"
  (box (1 :: Int))
  ifComparison

----------- new feature tests ------------

testEqu :: Test
testEqu = testEq "eq"
  (box True)
  eqSyntax

testCmp :: Test
testCmp = testEq
 "comparison"
  (box True)
  cmpSyntax

exprTests :: Test
exprTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEqu,
    testCmp
    ]
