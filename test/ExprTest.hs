module ExprTest where
import Bool.Effects (Cond)
import Utils.Composition (type (+), type (\/))
import Utils.Denote 
import Utils.Free (Free)
import Arith.Syntax as A 
import Arith.Denotation as A ( denote )
import Bool.Syntax as B 
import Bool.Denotation as B
import Expr.Denotation as E
import Expr.Syntax
import Utils.Handler (unwrap, handle)
import Bool.Handlers (condition)
import Test.HUnit 
import Utils.Fix ( Fix, injF ) 
import TestSyntax 
import Syntax (Type(..))
import Utils.Handler (End)
import Utils.Fix
import Utils.Composition
import Utils.Environment

type Eff    = Cond + End
type V      = Fix (LitBool + LitInt)
type Module = Arith + Boolean + Expr
type Out    = Bool \/ Int

run :: FreeEnv Eff V
  -> Either Bool Int
run e = case unwrap
    $ handle condition
    $ e $ Env {}
  of 
    (In (L (B.Lit val)))  -> Left val
    (In (R (A.Lit val)))  -> Right val

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
