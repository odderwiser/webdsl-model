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
import Eval.Syntax (Eval)
import Bool.Interface as B
import Arith.Interface as A
import Expr.Interface as Ex
import Eval.Interface as Ev
import Test.HUnit 
import TestSyntax
import Utils.Fix (injF)
import Utils.Fix
import Eval.Syntax

type Eff = MLState Address Val + Cond + End
type V =  LitB \/ LitAr
type Module = Arith + Boolean + Expr + Eval

run :: (Env -> Free Eff V) 
  -> Either Bool Int
run e = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv [])
    $ e []
  of 
    (Left (B.Lit val), _)  -> Left val
    (Right (A.Lit val), _) -> Right val

runWithEnv ::  (Env -> Free Eff V) 
  -> Env -> [(Address, Val)]
  -> Either Bool Int
runWithEnv e env store = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv store)
    $ e env
  of 
    (Left (B.Lit val), _)  -> Left val
    (Right (A.Lit val), _) -> Right val

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = Ex.denote

instance Denote Eval Eff V where
  denote = Ev.denote



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

----------- new feature tests ------------

testVar :: Test
testVar = TestCase (
  assertEqual "var with env"
  (Right 5)
  (runWithEnv 
    (foldD varSyntax) 
    [("x", 0)] 
    [(0, VInt $ A.Lit 3)]
  ))

varSyntax :: Fix Module
varSyntax = injF $
    OpArith Add 
        (injF $ Var "x") 
        (injF $ A.lit 2)

testVDecl :: Test
testVDecl = TestCase (
  assertEqual "vDecl"
  (Right 8)
  (run $ foldD vDeclSyntax
  ))

vDeclSyntax :: Fix Module
vDeclSyntax = injF $
    VDecl "x" (injF $ A.lit 4) Int 
    (injF $ OpArith Mul
        (injF $ Var "x") 
        (injF $ A.lit 2))

testVAssign :: Test
testVAssign = TestCase (
  assertEqual "vAssign"
  (Right 8)
  (run $ foldD vAssignSyntax
  ))

vAssignSyntax :: Fix Module
vAssignSyntax = injF $
    VDecl "x" (injF $ A.lit 4) Int 
    (injF $ VAssign "x" (injF $ A.lit 8) Int)


evalTests :: Test
evalTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEq,
    testCmp,
    testVar,
    testVDecl,
    testVAssign
    ]