{-# LANGUAGE TypeApplications #-}
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

type Eff = MLState Address V + Cond + End
type V =  Bool \/ Int \/ Null
type Module = Arith + Boolean + Expr + Eval

run :: (Env -> Free Eff V)
  -> Maybe (Either Bool Int)
run e = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv [])
    $ e []
  of
    (Left val, _)           -> Just $ Left val
    (Right (Left val), _)   -> Just $ Right val
    (Right (Right _), _) -> Nothing

runWithEnv ::  (Env -> Free Eff V)
  -> Env -> [(Address, V)]
  -> Maybe (Either Bool Int)
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



testIf :: Test
testIf = TestCase (assertEqual "add"
        (Just $ injV True)
        (run $ foldD ifSimple)
    )

testIfComplicated :: Test
testIfComplicated = TestCase (assertEqual "add"
        (Just $ injV True)
        (run $ foldD ifComplicated)
    )

testIfAB :: Test
testIfAB = TestCase (
  assertEqual "add"
  (Just $ Right 2)
  (run $ foldD ifSyntax)
  )

testIfComp :: Test
testIfComp = TestCase (
  assertEqual "add"
  (Just $ Right 1)
  (run $ foldD
  ifComparison))


testEq :: Test
testEq = TestCase (
  assertEqual "add"
  (Just $ Left True)
  (run $ foldD eqSyntax
  ))

testCmp :: Test
testCmp = TestCase (
  assertEqual "add"
  (Just $ Left True)
  (run $ foldD cmpSyntax
  ))

----------- new feature tests ------------

testVar :: Test
testVar = TestCase (
  assertEqual "var with env"
  (Just $ Right 5)
  (runWithEnv
    (foldD varSyntax)
    [("x", 0)]
    [(0, injV (3 :: Int))] 
  ))

varSyntax :: Fix Module
varSyntax = injF $
    OpArith Add
        (injF $ Var "x")
        (injF $ A.lit 2)

testVDecl :: Test
testVDecl = TestCase (
  assertEqual "vAssign"
  Nothing
  (run $ foldD vDeclSyntax
  ))

vDeclSyntax :: Fix Module
vDeclSyntax = injF $
  VDecl "x" $ injF $
  VAssign "x" (injF $ B.lit True) Bool


testVValDecl :: Test
testVValDecl = TestCase (
  assertEqual "vDecl"
  (Just $ Right 8)
  (run $ foldD vValDeclSyntax
  ))

vValDeclSyntax :: Fix Module
vValDeclSyntax = injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ OpArith Mul
        (injF $ Var "x")
        (injF $ A.lit 2))

testVAssign :: Test
testVAssign = TestCase (
  assertEqual "vAssign"
  Nothing
  (run $ foldD vAssignSyntax
  ))

vAssignSyntax :: Fix Module
vAssignSyntax = injF $
    VValDecl "x" (injF $ A.lit 4) Int
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
    testVValDecl,
    testVAssign
    ]