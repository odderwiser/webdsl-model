module Actions.LoopTest where
import Actions.Framework
import Actions.Syntax as Syn
import Syntax
import Utils
import Actions.Bool as B
import Actions.Arith as A
import Test.HUnit 

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

--------------------------------

testForLoop = testEq "forLoop"
  (A.lit 8)
  (varInit "x" (injA 4) $ cons
    (forAll "e1" (injC [true, true, false])
      (varAssign "x" (A.bin Add (injVar "x") (injA 2)) )
      [ Where (injVar "e1")]
    ) (injVar "x") :: Fix Module)

testOrderAsc = testEq "order by ascending"
  (A.lit 48)
  (varInit "x" (injA 1) $ cons
    (forAll "e1" (injC [injA 2, injA 1, injA 4])
      (varAssign "x" (A.bin Mul 
        (A.bin Add (injVar "x") (injVar "e1")) (injVar "e1"))  )
      [ OrdBy (injVar "e1") True]
    ) (injVar "x") :: Fix Module)

testOrderDesc = testEq "order by descending"
  (A.lit 45)
  (varInit "x" (injA 1) $ cons
    (forAll "e1" (injC [injA 2, injA 1, injA 4])
      (varAssign "x" (A.bin Mul 
        (A.bin Add (injVar "x") (injVar "e1")) (injVar "e1")) )
      [ OrdBy (injVar "e1") False]
    ) (injVar "x") :: Fix Module)

testLimit = testEq "order by descending"
  (A.lit 3)
  (varInit "x" (injA 0) $ cons
    (forAll "e1" (injC [injA 2, injA 1, injA 4])
      (varAssign "x" (A.bin Add (injVar "x") (injVar "e1")) )
      [ Limit (injA 2)]
    ) (injVar "x") :: Fix Module)

testOffset = testEq "order by descending"
  (A.lit 5)
  (varInit "x" (injA 0) $ cons
    (forAll "e1" (injC [injA 2, injA 1, injA 4])
      (varAssign "x" (A.bin Add (injVar "x") (injVar "e1")) )
      [ Offset (injA 1)]
    ) (injVar "x") :: Fix Module)

testLimitOffset = testEq "order by descending"
  (A.lit 1)
  (varInit "x" (injA 0) $ cons
    (forAll "e1" (injC [injA 2, injA 1, injA 4])
      (varAssign "x" (A.bin Add (injVar "x") (injVar "e1"))  )
      [Limit (injA 2), Offset (injA 1)]
    ) (injVar "x") :: Fix Module)

testAllFilters = testEq "order by descending"
  (A.lit 5)
  (varInit "x" (injA 0)  $ cons
    (forAll "e1" (injC [injA 2, injA 1, injA 4, injA 3, injA 6, injA 3])
      (varAssign "x" (A.bin Add (injVar "x") (injVar "e1")) )
      [Where (Syn.bin Lt (injVar "e1") (injA 4)), 
        OrdBy (injVar "e1") False, Limit (injA 3), Offset (injA 1)]
    ) (injVar "x") :: Fix Module)
  

testForCount = testEq "count ascending"
  (A.lit 6)
  (varInit "x" (injA 0) $ cons
    (forRange "e1" (injA 1) (injA 4)
      (varAssign "x" (A.bin Add (injVar "x") (injVar "e1")) )
    ) (injVar "x") :: Fix Module)

testForCountDown = testEq "count descending"
  (A.lit 9)
  (varInit "x" (injA 0) $ cons
    (forRange "e1" (injA 4) (injA 1)
      (varAssign "x" (A.bin Add (injVar "x") (injVar "e1")) )
    ) (injVar "x") :: Fix Module)

testWhile = testEq "while"
  (injF $ A.Lit 3)
  (injF $ 
  VValDecl "x" (injA 0) $ injF $ S
    (injF $ While (injF $ OpCmp Lt (injVar "x") (injA 3))
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injA 1)) )
    ) (injVar "x") :: Fix Module)

loopTests = TestList 
    [ testForLoop
    , testOrderAsc
    , testOrderDesc
    , testLimit
    , testOffset
    , testLimitOffset
    , testAllFilters
    , testForCount
    , testForCountDown
    , testWhile
    ]
