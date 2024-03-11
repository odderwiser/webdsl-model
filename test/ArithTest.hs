module ArithTest where
import Utils.Denote
import Utils.Free
import Arith.Interface as A
import Arith.Syntax
import Utils.Composition
import Test.HUnit
import Utils.Fix
import Utils.Handler

type Eff = End
type V = LitAr


runArith :: (Env -> Free Eff LitAr) -> Int
runArith e = 
    case unwrap
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
        (runArith $ foldD $ In (lit 1))
    )

testAddition :: Test
testAddition = TestCase (
        assertEqual "add"
        3
        (runArith $ foldD $ In 
        (bin Add (lit 1) (lit 2)))
    )

arithTests :: Test
arithTests = TestList [testSimple, testAddition]
