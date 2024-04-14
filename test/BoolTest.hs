module BoolTest where
import Utils.Denote
import Utils.Free
import Bool.Effects
import Bool.Syntax 
import Bool.Interface as B
import Utils.Composition
import Utils.Handler
import Bool.Handlers
import Test.HUnit
import Utils.Fix 
import TestSyntax (ifSimple, ifComplicated)

type Eff    = Cond + End
type V      = Bool
type Module = Boolean

instance Denote Boolean Eff Bool where
  denote :: Boolean (FreeEnv Eff Bool)
    -> FreeEnv Eff Bool
  denote = B.denote

run :: FreeEnv Eff Bool -> Bool
run e = unwrap
        $ handle condition
        $ e $ Env {}

testEq :: String -> V -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

testSimple :: Test
testSimple = testEq "terminal" True
    $ In (lit True)

testOr :: Test
testOr = testEq "add" True
    $ In (bin Or
        (lit False)
        (lit True))

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
