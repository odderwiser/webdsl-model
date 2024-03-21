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

type Eff = (Cond + End)

instance Denote Boolean Eff Bool where
  denote :: Boolean (Env -> Free Eff Bool)
    -> Env -> Free Eff Bool
  denote = B.denote

run :: (Env -> Free Eff Bool) -> Bool
run e = unwrap
        $ handle condition
        $ e []

testSimple :: Test
testSimple = TestCase (
        assertEqual "terminal"
        True
        (run $ foldD $ In (lit True))
    )

testOr :: Test
testOr = TestCase (
        assertEqual "add"
        True
        (run $ foldD $ In
        (bin Or
            (lit False)
            (lit True)
        ))
    )

testIf :: Test
testIf = TestCase (assertEqual "add" 
        True
        (run $ foldD ifSimple)
    )

testIfComplicated :: Test
testIfComplicated = TestCase (assertEqual "add"
        True
        (run $ foldD ifComplicated)
    )

boolTests :: Test
boolTests = TestList [
    testSimple, 
    testOr, 
    testIf, 
    testIfComplicated 
    ]
