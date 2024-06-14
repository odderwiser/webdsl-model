module Actions.StrTest where
import Utils
import Actions.Str as S
import Test.HUnit (Test (..), assertEqual)
import Actions.Effects (Abort(Abort))
import Actions.Values

type Eff    = End
type V      = Fix LitStr
type Module = Str
type Output = String

run :: FreeEnv Eff V -> Output
run e = case unwrap $ e $ Env {} of
  (In (Box str)) -> str

instance Denote Str Eff V where
  denote :: Str (FreeEnv  Eff V)
    -> FreeEnv Eff V
  denote = S.denote

testEq :: String -> Output -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax


testSimple :: Test
testSimple = testEq "literal" "helloworld" 
  $ str "helloworld"
  

testAddition :: Test
testAddition = testEq "add" "helloworld" 
  $ add (str "hello") (str "world")


strTests :: Test
strTests = TestList 
  [ testSimple
  , testAddition
  ]
