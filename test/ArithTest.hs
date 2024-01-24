module ArithTest where
import Utils.Denote
import Utils.Free
import Effects
import Arith.Interface
import Arith.Syntax
import Utils.Composition
import Test.HUnit
import Utils.Fix
import Utils.Handler
import Arith.Handlers


runArith :: (Env -> Free (Operation OpArith LitAr + End) LitAr) -> Int
runArith e = 
    case unwrap 
        $ handle binOp 
        $ e []
    of (Lit val) -> val

testSimple :: Test
testSimple = TestCase (
        assertEqual "terminal"
        1
        (runArith $ foldD $ In (LitAr (Lit 1)))
    )

testAddition :: Test
testAddition = TestCase (
        assertEqual "add"
        3
        (runArith $ foldD $ In 
        (bin Add 
            (LitAr (Lit 1)) 
            (LitAr (Lit 2)))
    )
    )

arithTests :: Test
arithTests = TestList [testAddition]
