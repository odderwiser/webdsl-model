module Actions.StrTest where
import Utils
import Actions.Str as S
import Test.HUnit (Test (..), assertEqual)
import Actions.Effects (Abort(Abort))
import Actions.Values
import qualified Actions.Arith as A

type Eff    = End
type V      = Fix (LitStr + A.LitInt)
type Module = Str 
type Output = V

run :: FreeEnv Eff V -> Output
run e = unwrap $ e $ Env {}

instance Denote Str Eff V where
  denote :: Str (FreeEnv  Eff V)
    -> FreeEnv Eff V
  denote = S.denote

testEq :: String -> Output -> Fix Module -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax


testSimple :: Test
testSimple = testEq "literal" (boxV "helloworld") 
  $ str "helloworld"
  

testAddition :: Test
testAddition = testEq "add"  (boxV "helloworld")  
  $ add (str "hello") (str "world")

testLength :: Test
testLength = testEq "add"  (A.boxI 10 )  
  $ S.length (str "helloworld") 

strTests :: Test
strTests = TestList 
  [ testSimple
  , testAddition
  , testLength
  ]
