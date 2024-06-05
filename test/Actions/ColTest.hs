module Actions.ColTest where
import Actions.Framework
import Utils
import Test.HUnit (Test (..), assertEqual)
import Actions.Bool as B
import Actions.Syntax as Syn
import Actions.Arith as A


testEq :: String -> Out -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

testInt = testEq
  "contains int"
  (B.lit True)
  (in' (int 1)
    (list [int  2
      , int  4
      , A.subtract (int 3) (int 2)
    ]) :: Fix Module)

testBool = testEq
  "contains bool"
  (B.lit False)
  (in' true (list
      [ false
      , B.and true false
      , neq (int 3) (int 3)
      ]) :: Fix Module)

testList = testEq
  "contains list"
  (B.lit True)
  (in' (list [int 1]) (list [ list []
    , list [A.subtract (int 3) (int 2)]
    , list [ A.add (int 2) (int 3)
      , multiply (int 3) (int 3)
    ]]) :: Fix Module)

testComprehension = testEq
  "comprehension"
  (injF [B.lit False, B.lit True])
  (lComp Nothing (gt (var "exp") (int 5))
    "exp"
    (list [int 1, A.add (int 3) (int 6)]) [] :: Fix Module)

testAnd = testEq
  "andList list"
  (B.lit True)
  (lComp (Just And) (var "exp") "exp"
      (list [ true
      , B.or true false
      , lt (int 2) (int 3)
      , gte (int 3) (int 3)
      ]) [] :: Fix Module)

testOr = testEq
  "orList list"
  (B.lit False)
  (lComp (Just Or) (var "exp") "exp"
    (list [ false
      , B.and true false
      , gt (int 2) (int 3)
      , neq (int 3) (int 3)
      ]) [] :: Fix Module)

colTests :: Test
colTests = TestList
    [ testInt
    , testBool
    , testList
    , testComprehension
    , testAnd
    , testOr
    ]
