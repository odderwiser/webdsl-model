module Actions.ExprTest where
import Utils
import Actions.Arith as A 
import Actions.Bool as B 
import Actions.Syntax
import Test.HUnit 
import TestSyntax 
import Syntax (Type(..))
import Actions.Framework

-- type Eff    = Cond + End
-- type V      = Fix (LitBool + LitInt)
-- type Module = Arith + Boolean + Expr
-- type Out    = Bool \/ Int

-- run :: FreeEnv Eff V
--   -> Either Bool Int
-- run e = case unwrap
--     $ handle condition
--     $ e $ Env {}
--   of 
--     (In (L (B.Lit val)))  -> Left val
--     (In (R (A.Lit val)))  -> Right val

-- instance Denote Arith Eff V where
--   denote :: Arith (FreeEnv Eff V) -> FreeEnv Eff V
--   denote = A.denote

-- instance Denote Boolean Eff V where
--   denote :: Boolean (FreeEnv Eff V) -> FreeEnv Eff V
--   denote = B.denote

-- instance Denote Expr Eff V where
--   denote = E.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

-------- regression tests ---------

testIf :: Test
testIf = testEq 
  "ifSimple" 
  (injF $  B.Lit True)
  ifSimple
    

testIfComplicated :: Test
testIfComplicated = testEq
  "ifComplicated"
  (injF $  B.Lit True)
  ifComplicated

testIfAB :: Test
testIfAB = testEq "ifAB"
  (injF $  A.Lit 2)
  ifSyntax

testIfComp :: Test
testIfComp = testEq
 "ifComparison"
  (injF $  A.Lit 1)
  ifComparison

----------- new feature tests ------------

testEqu :: Test
testEqu = testEq "eq"
  (injF $  B.Lit True)
  eqSyntax

testCmp :: Test
testCmp = testEq
 "comparison"
  (injF $  B.Lit True)
  cmpSyntax

exprTests :: Test
exprTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEqu,
    testCmp
    ]
