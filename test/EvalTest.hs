module EvalTest where
import Eval.Effects
import Syntax hiding (unwrap)
import Utils.Composition
import Bool.Effects (Cond)
import Utils.Composition
import Bool.Syntax as B
import Arith.Syntax as A
import Expr.Syntax
import Utils.Denote
import Utils.Free
import Eval.Handlers
import Utils.Handler
import Bool.Handlers
import Bool.Interface as B
import Arith.Interface as A
import Expr.Interface as Ex
import Eval.Interface as Ev
import Test.HUnit
import TestSyntax
import Utils.Fix
import Eval.Syntax

type Eff    = MLState Address V + Cond + End
type V      =  Bool \/ Int \/ Null
type Module = Arith + Boolean + Expr + Eval
type Out    = Maybe (Either Bool Int)

run :: FreeEnv Eff V
  -> Out
run e = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv [])
    $ e $ Env []
  of
    (Left val, _)           -> Just $ Left val
    (Right (Left val), _)   -> Just $ Right val
    (Right (Right _), _) -> Nothing

runWithEnv :: FreeEnv Eff V
  -> Env Eff V -> [(Address, V)]
  -> Out
runWithEnv e env store = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv store)
    $ e env
  of
    (Left val, _)           -> Just $ Left val
    (Right (Left val), _)   -> Just $ Right val
    (Right (Right _), _) -> Nothing

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = Ex.denote

instance Denote Eval Eff V where
  denote = Ev.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

testEqEnv :: Denote m Eff V 
  => String -> Out -> Fix m 
  -> Env Eff V -> [(Address, V)] -> Test
testEqEnv id res syntax env heap = TestCase 
  $ assertEqual id res 
  $ runWithEnv (foldD syntax) env heap

testIf :: Test
testIf = testEq 
  "eval if simple"
  (Just $ injV True)
  ifSimple

testIfComplicated :: Test
testIfComplicated = testEq  
  "if complicates"
  (Just $ injV True)
  ifComplicated

testIfAB :: Test
testIfAB = testEq
  "ifAB"
  (Just $ Right 2)
  ifSyntax

testIfComp :: Test
testIfComp = testEq
  "ifComparison"
  (Just $ Right 1)
  ifComparison


testEqu :: Test
testEqu = testEq
 "eq"
  (Just $ Left True)
  eqSyntax

testCmp :: Test
testCmp =  testEq 
  "cmp"
  (Just $ Left True)
  cmpSyntax

----------- new feature tests ------------

testVar :: Test
testVar = testEqEnv
 "var with env"
  (Just $ Right 5)
  varSyntax
  (Env [("x", 0)])
  [(0, injV (3 :: Int))] 

varSyntax :: Fix Module
varSyntax = injF $
    OpArith Add
        (injF $ Var "x")
        (injF $ A.lit 2)

testVDecl :: Test
testVDecl = testEq
 "vDecl"
  Nothing
  vDeclSyntax

vDeclSyntax :: Fix Module
vDeclSyntax = injF $
  VDecl "x" $ injF $
  VAssign "x" (injF $ B.lit True) Bool

testVValDecl :: Test
testVValDecl = testEq
 "vValDecl"
  (Just $ Right 8)
  vValDeclSyntax

vValDeclSyntax :: Fix Module
vValDeclSyntax = injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ OpArith Mul
        (injF $ Var "x")
        (injF $ A.lit 2))

testVAssign :: Test
testVAssign = testEq
 "vAssign"
  Nothing
  vAssignSyntax

vAssignSyntax :: Fix Module
vAssignSyntax = injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ VAssign "x" (injF $ A.lit 8) Int)

testTwoVarsA :: Test
testTwoVarsA = testEq "two variables"
  (Just $ Right 4)
  twoVarsASyntax

twoVarsASyntax :: Fix Module
twoVarsASyntax = injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ VValDecl "y" (injF $ A.lit 3) Int
    (injF $ Var "x"))

testTwoVarsB :: Test
testTwoVarsB = testEq
 "two variables"
  (Just $ Right 3)
  twoVarsBSyntax

twoVarsBSyntax :: Fix Module
twoVarsBSyntax = injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ VValDecl "y" (injF $ A.lit 3) Int
    (injF $ Var "y"))


evalTests :: Test
evalTests = TestList 
  [ testIf
  , testIfAB
  , testIfComp
  , testEqu
  , testCmp
  , testVar
  , testVDecl
  , testVValDecl
  , testVAssign
  , testTwoVarsA
  , testTwoVarsB
  ]
