module ArithTest where
import Utils.Denote
import Utils.Free
import Effects
import Arith.Interface as A
import Arith.Syntax
import Utils.Composition
import Test.HUnit
import Utils.Fix
import Utils.Handler
import Arith.Handlers

type Eff = (Operation OpArith + End)
type V = LitAr


runArith :: (Env -> Free Eff  LitAr) -> Int
runArith e = 
    case unwrap 
        $ handle binOp 
        $ e []
    of (Lit val) -> val

instance Denote Arith Eff LitAr where
  denote :: Arith (Env -> Free Eff LitAr)
    -> Env -> Free Eff LitAr
  denote = A.denote

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
arithTests = TestList [testSimple, testAddition]
