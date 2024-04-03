module ArithBoolTest where
import Utils.Denote
import Utils.Free
import Bool.Effects
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
type V = Either Bool Int
type Module = Arith + Boolean

run :: (Env Eff V -> Free Eff V)
  -> Either Bool Int
run e =
  case unwrap
    $ handle condition
    $ e 
    $ Env {}
  of
    (Left  val) -> Left val
    (Right val) -> Right val

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

testEq :: String -> V -> Fix Module -> Test
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
