module ArithBoolTest where
import Utils.Denote
import Utils.Free
import Effects
import Bool.Syntax as B
import Utils.Composition
import Arith.Syntax as A
import Test.HUnit
import Utils.Handler
import Bool.Handlers as B
import Arith.Handlers as A
import Utils.Fix (bin)
import Utils.Fix (Fix(In))
import Utils.Fix (injF)

runBA :: (Env -> Free (Cond + Operation OpB LitB + Operation OpArith LitAr + End) (Either LitB LitAr)) -> Bool
runBA e = 
    case unwrap 
        $ handle A.binOp
        $ handle B.binOp
        $ handle condition 
        $ e []
    of (Left (B.Lit val)) -> val

testIf :: Test
testIf = TestCase (
        assertEqual "add"
        True
        (runBA $ foldD 
        (injF $ If (In $ bin And 
            (LitB (B.Lit False)) 
            (LitB (B.Lit True)))
            (injF $ LitAr (A.Lit 1)) 
            (injF $ LitAr (A.Lit 2))))
    )

boolTests :: Test
boolTests = TestList [testIf]