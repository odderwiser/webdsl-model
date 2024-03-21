module ExprTest where
import Bool.Effects (Cond)
import Utils.Composition (type (+), type (\/), End)
import Utils.Denote (Env, Denote(denote), foldD)
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

type Eff = Cond + End
type V =  Bool \/ Int
type Module = Arith + Boolean + Expr

run :: (Env -> Free Eff V)
  -> Either Bool Int
run e = case unwrap
    $ handle condition
    $ e []
  of 
    (Left val)  -> Left val
    (Right val) -> Right val

instance Denote Arith Eff V where
  denote :: Arith (Env -> Free Eff V) -> Env -> Free Eff V
  denote = A.denote

instance Denote Boolean Eff V where
  denote :: Boolean (Env -> Free Eff V) -> Env -> Free Eff V
  denote = B.denote

instance Denote Expr Eff V where
  denote = E.denote

-------- regression tests ---------

testIf :: Test
testIf = TestCase (assertEqual "add" 
        (Left True)
        (run $ foldD ifSimple)
    )

testIfComplicated :: Test
testIfComplicated = TestCase (assertEqual "add"
        (Left True)
        (run $ foldD ifComplicated)
    )

testIfAB :: Test
testIfAB = TestCase (
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

----------- new feature tests ------------

testEq :: Test
testEq = TestCase (
  assertEqual "add"
  (Left True)
  (run $ foldD eqSyntax
  ))

testCmp :: Test
testCmp = TestCase (
  assertEqual "add"
  (Left True)
  (run $ foldD cmpSyntax
  ))

exprTests :: Test
exprTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEq,
    testCmp
    ]
