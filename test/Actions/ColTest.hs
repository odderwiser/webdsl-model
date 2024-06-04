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
  (in' (injA 1)
    (injC [injA  2
      , injA  4
      , A.bin Sub (injA 3) (injA 2)
    ]) :: Fix Module)

testBool = testEq
  "contains bool"
  (B.lit False)
  (in' true (injC
      [ false
      , B.bin   And true false
      , Syn.bin Neq (injA 3) (injA 3)
      ]) :: Fix Module)

testList = testEq
  "contains list"
  (B.lit True)
  (in'
    (injC [injA 1])
    (injC
      [ injC []
      , injC [A.bin Sub (injA 3) (injA 2)]
      , injC
        [ A.bin Add (injA 2) (injA 3)
        , A.bin Mul (injA 3) (injA 3)
        ]]) :: Fix Module)

testComprehension = testEq
  "comprehension"
  (injF [B.lit False, B.lit True])
  (lComp Nothing (Syn.bin Gt (injVar "exp") (injA 5))
    "exp"
    (injC [injA 1, A.bin Add (injA 3) (injA 6)]) [] :: Fix Module)

testAnd = testEq
  "andList list"
  (B.lit True)
  (lComp (Just And) (injVar "exp") "exp"
      (injC [ true
      , B.bin Or true false
      , Syn.bin Lt (injA 2) (injA 3)
      , Syn.bin Gte (injA 3) (injA 3)
      ]) [] :: Fix Module)

testOr = testEq
  "orList list"
  (B.lit False)
  (lComp (Just Or) (injVar "exp") "exp"
    (injC
      [ false
      , B.bin And true false
      , Syn.bin Gt (injA 2) (injA 3)
      , Syn.bin Neq (injA 3) (injA 3)
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
