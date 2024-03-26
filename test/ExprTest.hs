module ExprTest where
import Bool.Effects (Cond)
import Utils.Composition (type (+), type (\/), End)
import Utils.Denote 
import Utils.Free (Free)
import Arith.Syntax as A 
import Arith.Interface as A ( denote )
import Bool.Syntax as B 
import Bool.Interface as B
import Expr.Interface as E
import Expr.Syntax
import Utils.Handler (unwrap, handle)
import Bool.Handlers (condition)
import Test.HUnit 
import Utils.Fix ( Fix, injF ) 
import TestSyntax 
import Syntax (Type(..))

type Eff    = Cond + End
type V      =  Bool \/ Int
type Module = Arith + Boolean + Expr
type Out    = Either Bool Int

run :: FreeEnv Eff V
  -> Either Bool Int
run e = case unwrap
    $ handle condition
    $ e $ Env []
  of 
    (Left val)  -> Left val
    (Right val) -> Right val

instance Denote Arith Eff V where
  denote :: Arith (FreeEnv Eff V) -> FreeEnv Eff V
  denote = A.denote

instance Denote Boolean Eff V where
  denote :: Boolean (FreeEnv Eff V) -> FreeEnv Eff V
  denote = B.denote

instance Denote Expr Eff V where
  denote = E.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

-------- regression tests ---------

testIf :: Test
testIf = testEq 
  "ifSimple" 
  (Left True)
  ifSimple
    

testIfComplicated :: Test
testIfComplicated = testEq
  "ifComplicated"
  (Left True)
  ifComplicated

testIfAB :: Test
testIfAB = testEq "ifAB"
  (Right 2)
  ifSyntax

testIfComp :: Test
testIfComp = testEq
 "ifComparison"
  (Right 1)
  ifComparison

----------- new feature tests ------------

testEqu :: Test
testEqu = testEq "eq"
  (Left True)
  eqSyntax

testCmp :: Test
testCmp = testEq
 "comparison"
  (Left True)
  cmpSyntax

exprTests :: Test
exprTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEqu,
    testCmp
    ]
