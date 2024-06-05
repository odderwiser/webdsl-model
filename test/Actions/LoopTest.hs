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
  (varInit "x" (int 4) $ cons
    (forAll "e1" (list [true, true, false])
      (varAssign "x" (A.add (var "x") (int 2)) )
      [ Where (var "e1")]
    ) (var "x") :: Fix Module)

testOrderAsc = testEq "order by ascending"
  (A.lit 48)
  (varInit "x" (int 1) $ cons
    (forAll "e1" (list [int 2, int 1, int 4])
      (varAssign "x" (multiply 
        (A.add (var "x") (var "e1")) (var "e1"))  )
      [ OrdBy (var "e1") True]
    ) (var "x") :: Fix Module)

testOrderDesc = testEq "order by descending"
  (A.lit 45)
  (varInit "x" (int 1) $ cons
    (forAll "e1" (list [int 2, int 1, int 4])
      (varAssign "x" (multiply 
        (A.add (var "x") (var "e1")) (var "e1")) )
      [ OrdBy (var "e1") False]
    ) (var "x") :: Fix Module)

testLimit = testEq "order by descending"
  (A.lit 3)
  (varInit "x" (int 0) $ cons
    (forAll "e1" (list [int 2, int 1, int 4])
      (varAssign "x" (A.add (var "x") (var "e1")) )
      [ Limit (int 2)]
    ) (var "x") :: Fix Module)

testOffset = testEq "order by descending"
  (A.lit 5)
  (varInit "x" (int 0) $ cons
    (forAll "e1" (list [int 2, int 1, int 4])
      (varAssign "x" (A.add (var "x") (var "e1")) )
      [ Offset (int 1)]
    ) (var "x") :: Fix Module)

testLimitOffset = testEq "order by descending"
  (A.lit 1)
  (varInit "x" (int 0) $ cons
    (forAll "e1" (list [int 2, int 1, int 4])
      (varAssign "x" (A.add (var "x") (var "e1"))  )
      [Limit (int 2), Offset (int 1)]
    ) (var "x") :: Fix Module)

testAllFilters = testEq "order by descending"
  (A.lit 5)
  (varInit "x" (int 0)  $ cons
    (forAll "e1" (list $ map int [2, 1, 4, 3, 6, 3])
      (varAssign "x" (A.add (var "x") (var "e1")) )
      [Where (lt (var "e1") (int 4)), 
        OrdBy (var "e1") False, Limit (int 3), Offset (int 1)]
    ) (var "x") :: Fix Module)
  

testForCount = testEq "count ascending"
  (A.lit 6)
  (varInit "x" (int 0) $ cons
    (forRange "e1" (int 1) (int 4)
      (varAssign "x" (A.add (var "x") (var "e1")))
    ) (var "x") :: Fix Module)

testForCountDown = testEq "count descending"
  (A.lit 9)
  (varInit "x" (int 0) $ cons
    (forRange "e1" (int 4) (int 1)
      (varAssign "x" (A.add (var "x") (var "e1")) )
    ) (var "x") :: Fix Module)

testWhile = testEq "while"
  (injF $ A.Lit 3)
  (injF $ 
  VValDecl "x" (int 0) $ cons
    (while (lt (var "x") (int 3))
      (varAssign "x" (A.add (var "x") (int 1)) )
    ) (var "x") :: Fix Module)

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
