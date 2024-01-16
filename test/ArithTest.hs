module ArithTest where
import Arith
import Infrastructure
import Free hiding (fold)
import Effects
import Test.HUnit 

runArith :: (Env -> Free (Operation OpArith LitAr + End) LitAr) -> Int
runArith e = 
    case unwrap 
        $ handle Arith.binOp 
        $ e []
    of (Lit val) -> val

 
-- evalContainer :: Env -> (Env -> Free (Operation OpArith LitAr + End) LitAr) 
--     -> Free (Operation OpArith LitAr + End) LitAr
-- evalContainer env e =  fold 

fold :: Denote f eff v => Fix f -> (Env -> Free eff v)
fold (In f) = denote $ fmap fold f 

testSimple :: Test
testSimple = TestCase (
        assertEqual "terminal"
        1
        (runArith $ fold $ In (LitAr (Lit 1)))
    )

testAddition :: Test
testAddition = TestCase (
        assertEqual "add"
        3
        (runArith $ fold (bin Add (LitAr (Lit 1)) (LitAr (Lit 2)) :: Fix Arith))
    )

arithTests :: Test
arithTests = TestList [testAddition]