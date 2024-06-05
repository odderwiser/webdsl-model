module Actions.BoolTest where
import Utils
import Actions.Effects
import Actions.Bool as B 
import Actions.Handlers.Cond
import Test.HUnit
import TestSyntax (ifSimple, ifComplicated)

type Eff    = Cond + End
type V      = Fix LitBool
type Module = Boolean
type Output = Bool

instance Denote Boolean Eff V where
  denote :: Boolean (FreeEnv Eff V)
    -> FreeEnv Eff V
  denote = B.denote

run :: FreeEnv Eff V -> Bool
run e = case unwrap
    $ handle condition
    $ e $ Env {}
  of
    (In (Lit bool)) -> bool

testEq :: String -> Output -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

testSimple :: Test
testSimple = testEq "terminal" True
  true

testOr :: Test
testOr = testEq "add" True
  $ B.or false true

testIf :: Test
testIf = testEq "add" True ifSimple

testIfComplicated :: Test
testIfComplicated = testEq "add" True ifComplicated

boolTests :: Test
boolTests = TestList 
    [ testSimple
    , testOr
    , testIf
    , testIfComplicated 
    ]
