module Templates.PageTest where

import Templates.Framework as Tp
import Test.HUnit as T
import Utils
import Definitions.Program.Syntax
import Definitions.Pages.Framework
import Syntax
import Definitions.Pages.Syntax (PageCall)
import Actions.Framework
import Templates.Syntax
import Actions.Arith as A
import Actions.Syntax
import Definitions.Templates.Framework (tDefEnv)
import Definitions.Pages.Syntax

testEq :: ()
  => String -> Out' -> Module' -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ Tp.run $ foldDT syntax

testEqProgram :: String -> Out'
    -> Program DefSyntax (PageCall (Fix Module) Module') -> T.Test
testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldProgram syntax

defsSyn :: [DefSyntax]
defsSyn = [
    pDefEnv "root" [] 
      [ tCall "inside" [(A.add (int 2) (int 1), Int)]],
    tDefEnv "inside" [("a", Int)] 
      $ output $ var "a" 
    ]

pCallSyntax :: Program'
pCallSyntax = Fragment defsSyn pCallRoot

testPCall = testEqProgram "page Call"
    (   "<html><head></head><body id=\"root\">"
     ++ "3</body></html>")
     pCallSyntax

properProgramSyntax = Program defsSyn

testProperProgram = testEqProgram "root page" 
    (   "<html><head></head><body id=\"root\">"
     ++ "3</body></html>")
     properProgramSyntax

pageTests = T.TestList 
  [ testPCall
  , testProperProgram
  ]
