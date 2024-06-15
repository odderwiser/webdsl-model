{-# OPTIONS_GHC -Wno-missing-fields #-}
module Actions.ArithTest where
import Utils
import Actions.Modules.Arith.Syntax as A
import Actions.Modules.Arith.Denotation as A
import Test.HUnit 
import Actions.Values

type Eff    = End
type V      = Fix LitInt
type Module = Arith
type Output = Int


run :: FreeEnv Eff V -> Maybe Int
run e = case unwrap $ e $ Env {} of
  (In (V int)) -> Just int
  _ -> Nothing

instance Denote Arith Eff V where
  denote :: Arith (FreeEnv  Eff V)
    -> FreeEnv Eff V
  denote = A.denote

testEq :: String -> Int -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id (Just res) $ run $ foldD syntax


testSimple :: Test
testSimple = testEq "literal" 1 
  $ int 1
  

testAddition :: Test
testAddition = testEq "add" 3 
  $ add (int 1) (int 2)


arithTests :: Test
arithTests = TestList 
  [ testSimple
  , testAddition
  ]
