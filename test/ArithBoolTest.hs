module ArithBoolTest where
import Utils.Denote
import Utils.Free
import Bool.Effects
import Bool.Syntax as B
import Utils.Composition
import Arith.Syntax as A
import Arith.Denotation as A (denote)
import Test.HUnit
import Utils.Handler
import Bool.Handlers as B
import Bool.Denotation as B 
import Utils.Fix ( bin, Fix(In), injF )
import TestSyntax (ifSyntax, ifComparison)
import Utils.Environment

type Eff = Cond + End
type V = Fix (LitBool + LitInt)
type Module = Arith + Boolean
type Output = Bool \/ Int

run :: (Env Eff V -> Free Eff V)
  -> Either Bool Int
run e =
  case unwrap
    $ handle condition
    $ e 
    $ Env {}
  of
    (In (L (B.Lit val))) -> Left val
    (In (R (A.Lit val))) -> Right val

instance Denote Arith Eff V where
  denote :: Arith (FreeEnv Eff V) -> FreeEnv Eff V
  denote = A.denote

instance Denote Boolean Eff V where
  denote :: Boolean (FreeEnv Eff V) -> FreeEnv Eff V
  denote = B.denote

testEq :: String -> Output -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

testIf :: Test
testIf = testEq
  "add"
  (Right 2)
  ifSyntax

testIfComp :: Test
testIfComp = testEq "add"
  (Right 1)
  ifComparison

arithBoolTests :: Test
arithBoolTests = TestList 
  [ testIf
  , testIfComp
  ]
