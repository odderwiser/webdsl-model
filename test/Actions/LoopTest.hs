module Actions.LoopTest where
import Actions.Framework
import Actions.Syntax
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
  (injF $ A.Lit 8)
  (injF $ 
  VValDecl "x" (injF $ A.lit 4) $ injF $ S
    (injF $ ForCol "e1" (injC [injB True, injB True, injB False])
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injA 2)) )
      [ Where (injVar "e1")]
    ) (injVar "x") :: Fix Module)

testOrderAsc = testEq "order by ascending"
  (injF $ A.Lit 48)
  (injF $ 
  VValDecl "x" (injF $ A.lit 1) $ injF $ S
    (injF $ ForCol "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign "x" (injF $ OpArith Mul (injF 
        $ OpArith Add (injVar "x") (injVar "e1")) (injVar "e1"))  )
      [ OrdBy (injVar "e1") True]
    ) (injVar "x") :: Fix Module)

testOrderDesc = testEq "order by descending"
  (injF $ A.Lit 45)
  (injF $ 
  VValDecl "x" (injF $ A.lit 1) $ injF $ S
    (injF $ ForCol "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign "x" (injF $ OpArith Mul (injF 
        $ OpArith Add (injVar "x") (injVar "e1")) (injVar "e1")) )
      [ OrdBy (injVar "e1") False]
    ) (injVar "x") :: Fix Module)

testLimit = testEq "order by descending"
  (injF $ A.Lit 3)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0) $ injF $ S
    (injF $ ForCol "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injVar "e1")) )
      [ Limit (injA 2)]
    ) (injVar "x") :: Fix Module)

testOffset = testEq "order by descending"
  (injF $ A.Lit 5)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0) $ injF $ S
    (injF $ ForCol "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injVar "e1")) )
      [ Offset (injA 1)]
    ) (injVar "x") :: Fix Module)

testLimitOffset = testEq "order by descending"
  (injF $ A.Lit 1)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0) $ injF $ S
    (injF $ ForCol "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injVar "e1"))  )
      [Limit (injA 2), Offset (injA 1)]
    ) (injVar "x") :: Fix Module)

testAllFilters = testEq "order by descending"
  (injF $ A.Lit 5)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0)  $ injF $ S
    (injF $ ForCol "e1" (injC [injA 2, injA 1, injA 4, injA 3, injA 6, injA 3])
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injVar "e1")) )
      [Where (injF $ OpCmp Lt (injVar "e1") (injA 4)), 
        OrdBy (injVar "e1") False, Limit (injA 3), Offset (injA 1)]
    ) (injVar "x") :: Fix Module)
  

testForCount = testEq "count ascending"
  (injF $ A.Lit 6)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0) $ injF $ S
    (injF $ ForArith "e1" (injA 1) (injA 4)
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injVar "e1")) )
    ) (injVar "x") :: Fix Module)

testForCountDown = testEq "count descending"
  (injF $ A.Lit 9)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0) $ injF $ S
    (injF $ ForArith "e1" (injA 4) (injA 1)
      (injF $ VAssign "x" (injF $ OpArith Add (injVar "x") (injVar "e1")) )
    ) (injVar "x") :: Fix Module)

testWhile = testEq "while"
  (injF $ A.Lit 3)
  (injF $ 
  VValDecl "x" (injF $ A.lit 0) $ injF $ S
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
