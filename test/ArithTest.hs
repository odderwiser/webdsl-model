{-# OPTIONS_GHC -Wno-missing-fields #-}
module ArithTest where
import Utils.Denote
import Utils.Free
import Arith.Interface as A
import Arith.Syntax
import Utils.Composition
import Test.HUnit 
import Utils.Fix
import Utils.Handler

type Eff    = End
type V      = Int
type Module = Arith


run :: (Env Eff V -> Free Eff Int) -> Int
run e = unwrap $ e $ Env {}

instance Denote Arith Eff Int where
  denote :: Arith (Env Eff V -> Free Eff Int)
    -> Env Eff V -> Free Eff Int
  denote = A.denote

testEq :: String -> V -> Fix Module -> Test
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
