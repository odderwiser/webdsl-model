module ExprTest where
import Utils.Denote
import Utils.Free
import Effects
import Bool.Syntax as B
import Bool.Handlers
import Utils.Composition
import Arith.Syntax as A
import Arith.Handlers
import Test.HUnit
import Utils.Fix
import Utils.Handler

runExpr :: (Env 
    -> Free (Cond (LitB e) 
        + Operation OpB (LitB e) 
        + Operation OpArith (LitAr e) 
        + End) ((LitB + LitAr) e)) 
    -> Either Bool Int
runExpr e = 
    case unwrap 
        $ handle Arith.Handlers.binOp
        $ handle Bool.Handlers.binOp
        $ handle condition 
        $ e []
    of 
        (L (B.Lit val)) -> Left val
        (R (A.Lit val)) -> Right val

testIf :: Test
testIf = TestCase (
        assertEqual "add"
        (Right 1)
        (runExpr $ foldD 
        (ifTE (bin Or 
            (LitB (B.Lit False)) 
            (LitB (B.Lit True)))
            (LitAr (A.Lit 1)) 
            (LitAr (A.Lit 2))))
    )

boolTests :: Test
boolTests = TestList [testIf]