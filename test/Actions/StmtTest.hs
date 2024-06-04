module Actions.StmtTest where
import Syntax
import Utils
import Actions.Framework
import Test.HUnit
import Actions.Arith as A 
import Actions.Bool as B 
import Actions.Syntax

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

--------------------------------

testStmt :: Test
testStmt = testEq "Stmt"
  (injF $ A.Lit 8)
  stmtSyntax

stmtSyntax :: Fix Module
stmtSyntax = injF $ 
  VValDecl "x" (injA 4)
    (injF $ S 
      (injF $ VAssign "x" (injA 8))
      (injVar "x")
  )

stmtTests :: Test
stmtTests = TestList 
  [ testStmt
  ]
