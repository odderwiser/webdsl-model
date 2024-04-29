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
import Bool.Denotation as B (denote)
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
  denote :: Arith (FreeEnv Eff V) -> FreeEnv Eff V
  denote = A.denote

instance Denote Boolean Eff V where
  denote :: Boolean (FreeEnv Eff V) -> FreeEnv Eff V
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
