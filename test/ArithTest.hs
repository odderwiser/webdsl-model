{-# OPTIONS_GHC -Wno-missing-fields #-}
module ArithTest where
import Utils.Denote
import Utils.Free
import Arith.Denotation as A
import Arith.Syntax
import Utils.Composition
import Test.HUnit 
import Utils.Fix
import Utils.Handler

type Eff    = End
type V      = Fix LitInt
type Module = Arith
type Output = Int


run :: FreeEnv Eff V -> Int
run e = case unwrap $ e $ Env {} of
  (In (Lit int)) -> int

instance Denote Arith Eff V where
  denote :: Arith (FreeEnv  Eff V)
    -> FreeEnv Eff V
  denote = A.denote

testEq :: String -> Output -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax


testSimple :: Test
testSimple = testEq "literal" 1 
  $ injF $ lit 1
  

testAddition :: Test
testAddition = testEq "add" 3 
  $ In $ bin Add (lit 1) (lit 2)


arithTests :: Test
arithTests = TestList 
  [ testSimple
  , testAddition
  ]
