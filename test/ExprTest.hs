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
import Utils.Fix (injF)
import TestSyntax (ifSimple, ifSyntax, ifComplicated, ifComparison)
import Syntax (Type(..))
import Utils.Fix

type Eff = Cond + End
type V =  LitB \/ LitAr
type Module = Arith + Boolean + Expr

run :: (Env -> Free Eff V)
  -> Either Bool Int
run e = case unwrap
    $ handle condition
    $ e []
  of 
    (Left (B.Lit val))  -> Left val
    (Right (A.Lit val)) -> Right val

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
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

eqSyntax :: Fix Module
eqSyntax = injF 
    $ If (injF $ OpCmp Eq 
        (injF $ B.lit True, Bool) 
        (injF $ B.lit False, Bool)) 
        (injF $ OpCmp Eq 
            (injF $ B.lit True, Bool) 
            (injF $ B.lit True, Int)) -- wrong on purpose
        (injF $ OpCmp Neq 
            (injF $ A.lit 1, Int) 
            (injF $ A.lit 2, Int))

testCmp :: Test
testCmp = TestCase (
  assertEqual "add"
  (Left True)
  (run $ foldD cmpSyntax
  ))

cmpSyntax :: Fix Module
cmpSyntax = injF 
    $ If (injF $ OpCmp Lt
        (injF $ A.lit 1, Int) 
        (injF $ A.lit 2, Int)) 
        (injF $ OpCmp Lte 
            (injF $ A.lit 3, Int) 
            (injF $ A.lit 3, Int)) 
        (injF $ OpCmp Gt 
            (injF $ A.lit 1, Int) 
            (injF $ A.lit 1, Int))

exprTests :: Test
exprTests = TestList [
    testIf, 
    testIfAB,
    testIfComp,
    testEq,
    testCmp
    ]
