module Actions.ColTest where
import Actions.Framework
import Utils
import Test.HUnit (Test (..), assertEqual)
import Actions.Bool as B 
import Actions.Syntax
import Actions.Arith as A


testEq :: String -> Out -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

testInt = testEq
  "contains int"
  (injF $ B.Lit True)
  (injF $ OpIn 
    (injA 1) 
    (injC  
      [ injA  2
      , injA  4
      , injF $ OpArith Sub (injA 3) (injA 2)
      ]) :: Fix Module) 

testBool = testEq
  "contains bool"
  (injF $ B.Lit False)
  (injF $ OpIn
    (injB True) 
    (injC 
      [ injB False
      , injF $ OpB   And (injB True) (injB False)
      , injF $ OpCmp Neq (injA 3) (injA 3)
      ]) :: Fix Module) 

testList = testEq
  "contains list"
  (injF $ B.Lit True)
  (injF $ OpIn
    (injC [injA 1]) 
    (injC 
      [ injC []
      , injC [injF $ OpArith Sub (injA 3) (injA 2)]
      , injC 
        [ injF $ OpArith Add (injA 2) (injA 3)
        , injF $ OpArith Mul (injA 3) (injA 3)
        ]]) :: Fix Module)

testComprehension = testEq
  "comprehension"
  (injF [injF $ B.Lit False, injF $ B.Lit True])
  ((injF $ LComp (injF $ OpCmp Gt (injVar "exp") (injA 5))
    "exp"
    (injC [injA 1, injF $ OpArith Add (injA 3) (injA 6)]) []) :: Fix Module)

testAnd = testEq
  "andList list"
  (injF $ B.Lit True)
  (injF $ UnOp And
    (injC 
      [ injB True
      , injF $ OpB Or (injB True) (injB False)
      , injF $ OpCmp Lt (injA 2) (injA 3)
      , injF $ OpCmp Gte (injA 3) (injA 3)
      ]) :: Fix Module)

testOr = testEq
  "orList list"
  (injF $ B.Lit False)
  (injF $ UnOp Or
    (injC 
      [ injB False
      , injF $ OpB And (injB True) (injB False)
      , injF $ OpCmp Gt (injA 2) (injA 3)
      , injF $ OpCmp Neq (injA 3) (injA 3)
      ]) :: Fix Module)

colTests :: Test
colTests = TestList 
    [ testInt
    , testBool
    , testList
    , testComprehension
    , testAnd
    , testOr
    ]
