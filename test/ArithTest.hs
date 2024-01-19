module ArithTest where
import Effects
import Test.HUnit
import Arith.Syntax
import Arith.Handlers (binOp)
import Arith.Interface
import Utils.Denote ( Env, foldD, coerceFree )
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix
import Utils.Handler

runArith :: (Env -> Free (Operation OpArith (LitAr e) + End) (LitAr e)) -> Int
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
            (LitAr (Lit 2))
        ))
    )

arithTests :: Test
arithTests = TestList [testSimple,
 testAddition]
