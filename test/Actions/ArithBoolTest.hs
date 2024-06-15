module Actions.ArithBoolTest where
import Utils
import Actions.Effects
import Test.HUnit
import Actions.Handlers.Cond
import TestSyntax (ifSyntax, ifComparison)
import Actions.Bool  as B
import Actions.Arith as A
import Actions.Values

type Eff = Cond + End
type V = Fix (LitBool + LitInt)
type Module = Arith + Boolean
type Output = Bool \/ Int

run :: (Env Eff V -> Free Eff V)
  -> Either Bool Int
run e =
  case unwrap
    $ handle condition
    $ e 
    $ Env {}
  of
    (In (L (V val))) -> Left val
    (In (R (V val))) -> Right val

instance Denote Arith Eff V where
  denote :: Arith (FreeEnv Eff V) -> FreeEnv Eff V
  denote = A.denote

instance Denote Boolean Eff V where
  denote :: Boolean (FreeEnv Eff V) -> FreeEnv Eff V
  denote = B.denote

testEq :: String -> Output -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

testIf :: Test
testIf = testEq
  "add"
  (Right 2)
  ifSyntax

testIfComp :: Test
testIfComp = testEq "add"
  (Right 1)
  ifComparison

arithBoolTests :: Test
arithBoolTests = TestList 
  [ testIf
  , testIfComp
  ]
