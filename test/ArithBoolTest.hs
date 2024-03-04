module ArithBoolTest where
import Utils.Denote
import Utils.Free
import Effects
import Bool.Syntax as B
import Utils.Composition
import Arith.Syntax as A
import Arith.Interface as A (denote)
import Test.HUnit
import Utils.Handler
import Bool.Handlers as B
import Arith.Handlers as A
import Bool.Interface as B (denote)
import Utils.Fix (bin)
import Utils.Fix (Fix(In))
import Utils.Fix (injF)

type Eff = Cond + Operation OpB + Operation OpArith + End
type V = Either LitB LitAr

runBA :: (Env
  -> Free Eff V)
  -> Either Bool Int
runBA e =
  case unwrap
    $ handle A.binOp
    $ handle B.binOp
    $ handle condition
    $ e []
  of (Left (B.Lit val)) -> Left val

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

testIf :: Test
testIf = TestCase (
  assertEqual "add"
  (Right 2)
  (runBA $ foldD
  (injF $ If (injF (LitB (B.Lit False)))
      (injF (LitAr (A.Lit 1)))
      (injF  (LitAr (A.Lit 2)) :: Fix (Arith + Boolean))
  ))
  )

testIfComp :: Test
testIfComp = TestCase (
  assertEqual "add"
  (Right 1)
  (runBA $ foldD
  (injF $ If (injF (OpB
      Or
        (injF $ LitB (B.Lit False))
        (injF $ LitB (B.Lit True))))
      (injF (LitAr (A.Lit 1)))
      (injF  (LitAr (A.Lit 2)) :: Fix (Arith + Boolean))
  )))

arithBoolTests :: Test
arithBoolTests = TestList [testIf, testIfComp]
