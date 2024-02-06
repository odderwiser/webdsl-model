module ArithBoolTest where
import Utils.Denote
import Utils.Free
import Effects
import Bool.Syntax as B
import Utils.Composition
import Arith.Syntax as A
import Arith.Interface
import Test.HUnit
import Utils.Handler
import Bool.Handlers as B
import Arith.Handlers as A
import Utils.Fix (bin)
import Utils.Fix (Fix(In))
import Utils.Fix (injF)

runBA :: (Env -> Free (Cond + Operation OpB LitB + Operation OpArith LitAr + End) (Either LitB LitAr)) -> Either Bool Int
runBA e = 
    case unwrap 
        $ handle A.binOp
        $ handle B.binOp
        $ handle condition 
        $ e []
    of (Left (B.Lit val)) -> Left val

testIf :: Test
testIf = TestCase (
        assertEqual "add"
        (Right 2)
        (runBA $ foldD 
        (In (inj $ If (injF (LitB (B.Lit False)) :: Fix (Arith + Boolean))
            (injF (LitAr (A.Lit 1)) :: Fix (Arith + Boolean)) 
            (injF  (LitAr (A.Lit 2)) :: Fix (Arith + Boolean)))
        ))
    )

boolTests :: Test
boolTests = TestList [testIf]