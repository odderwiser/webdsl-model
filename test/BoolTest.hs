module BoolTest where
import Utils.Denote
import Utils.Free
import Effects
import Bool.Syntax ( Boolean(LitB, If), OpB(Or, And), LitB(..) )
import Bool.Interface 
import Utils.Composition
import Utils.Handler
import Bool.Handlers
import Test.HUnit
import Utils.Fix

runBool :: (Env -> Free (Cond + Operation OpB LitB + End) LitB) -> Bool
runBool e = 
    case unwrap 
        $ handle binOp
        $ handle condition 
        $ e []
    of (Lit val) -> val

testSimple :: Test
testSimple = TestCase (
        assertEqual "terminal"
        True
        (runBool $ foldD $ In (LitB (Lit True)))
    )

testOr :: Test
testOr = TestCase (
        assertEqual "add"
        True
        (runBool $ foldD $ In
        (bin Or 
            (LitB (Lit False)) 
            (LitB (Lit True)) 
        ))
    )

testIf :: Test
testIf = TestCase (
        assertEqual "add"
        True
        (runBool $ foldD 
        (In $ If (injF $ LitB (Lit False))
            (injF $ LitB (Lit False)) 
            (injF $ LitB (Lit True))))
    )

testIfComplicated :: Test
testIfComplicated = TestCase (
        assertEqual "add"
        True
        (runBool $ foldD 
        (In $ If (In $ bin And 
            (LitB (Lit False)) 
            (LitB (Lit True)))
            (injF $ LitB (Lit False)) 
            (injF $ LitB (Lit True))))
    )

boolTests :: Test
boolTests = TestList [testSimple, testOr, testIf, testIfComplicated]
