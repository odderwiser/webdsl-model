{-# OPTIONS_GHC -Wno-missing-fields #-}
module Actions.ArithTest where
import Utils
import Actions.Modules.Arith.Syntax as A
import Actions.Modules.Arith.Denotation as A
import Test.HUnit 

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

testEq :: String -> Int -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax


testSimple :: Test
testSimple = testEq "literal" 1 
  $ injA 1
  

testAddition :: Test
testAddition = testEq "add" 3 
  $ injF $ OpArith Add (injA 1) (injA 2)


arithTests :: Test
arithTests = TestList 
  [ testSimple
  , testAddition
  ]
