module Templates.PageTest where
import Templates.Framework as Tp
import Test.HUnit as T
import Utils.Denote
import Definitions.Templates.Framework
import Definitions.Program.Syntax
import Utils
import Templates.Syntax
import Syntax as S
import Definitions.Templates.Syntax
import Actions.Arith as A
import Actions.Syntax as A
import Actions.Str as As

testEq :: ()
  => String -> Out' -> Module' -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ Tp.run $ foldDT syntax

testEqProgram :: String -> Out'
    -> Program DefSyntax Module' -> T.Test
testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldTProgram syntax


defsSyn :: [DefSyntax]
defsSyn = [
    tDefEnv "nestedVars" [("a", Int), ("b", S.String)] 
      $ tCall "inside" [(A.add (var "a") (int 1), Int)],
    tDefEnv "inside" [("a", Int)] 
      $ output $ var "a" 
    ]

syntax :: Program DefSyntax Module'
syntax = Fragment defsSyn $ section False 
  $ tCall "nestedVars" [(int 5, Int), (As.str "a", S.String)]

testSyntax = testEqProgram "test TCall"
  (   "<html><head></head><body>"
     ++ "<span class=\"section section1\">"
     ++ "6</span></body></html>")
  syntax

pageTests = T.TestList [
  testSyntax
  ]