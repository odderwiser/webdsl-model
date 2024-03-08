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
import Bool.Interface as B (denote)
import Utils.Fix ( bin, Fix(In), injF )
import TestSyntax (ifSyntax, ifComparison)

type Eff = Cond + End
type V = Either LitB LitAr

run :: (Env -> Free Eff V)
  -> Either Bool Int
run e =
  case unwrap
    $ handle condition
    $ e []
  of
    (Left (B.Lit val))  -> Left val
    (Right (A.Lit val)) -> Right val

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

testIf :: Test
testIf = TestCase (
  assertEqual "add"
  (Right 2)
  (run $ foldD ifSyntax)
  )

testIfComp :: Test
testIfComp = TestCase (
  assertEqual "add"
  (Right 1)
  (run $ foldD
  ifComparison))

arithBoolTests :: Test
arithBoolTests = TestList [testIf, testIfComp]
