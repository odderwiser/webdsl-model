module Templates.PageTest where

import Templates.FrameworkIO as Tp
import Test.HUnit as T
import Utils
import Definitions.Program.Syntax
import Definitions.Pages.Framework
import Syntax
import Definitions.Pages.Syntax (PageCall)
import Actions.FrameworkIO
import Templates.Syntax
import Actions.Arith as A
import Actions.Syntax
import Definitions.Templates.Framework (tDefEnv)
import Definitions.Pages.Syntax
import Definitions.Templates.Syntax
    ( tDef, StatementType(..), body )
import Definitions.Templates.Framework (EnvTy)
import System.Directory (removeFile)

-- testEq :: ()
--   => String -> Out' -> Module' -> T.Test
-- testEq id res syntax =  T.TestCase $
--   T.assertEqual id res $ Tp.run (foldDT syntax) 

type Program'' = Program (Envs (PEnv (EffV V') (Eff' V') V) (EnvTy V')) ()
  (PageCall (PEnv (EffV V') (Eff' V') V) (EnvTy V'))

testEqProgram :: ()
  => String -> Out' -> Program DefSyntax () (PageCall Module' (Fix Module)) -> IO T.Test
testEqProgram id res syntax = do
  let file = "./test/Templates/dbs/pgs/"++id++ ".txt"
  -- removeFile file
  output <- runProgram (foldProgram syntax :: Program'') file
  return $ T.TestCase $
    T.assertEqual id res $ output

defsSyn :: [DefSyntax]
defsSyn = [
  pDef "root" [] $ tCall "inside" [(A.add (int 2) (int 1), Int)],
  tDef "inside" [("a", Int)] $ output $ var "a"
  ]

pCallSyntax :: Program DefSyntax () (PageCall Module' (Fix Module))
pCallSyntax = Fragment defsSyn Nothing pCallRoot

testPCall = testEqProgram "page Call"
    (   "<html><head></head><body id=\"root\">"
     ++ "3</body></html>")
     pCallSyntax

properProgramSyntax :: Program DefSyntax () (PageCall Module' (Fix Module))
properProgramSyntax = Program defsSyn Nothing

testProperProgram = testEqProgram "root page"
    (   "<html><head></head><body id=\"root\">"
     ++ "3</body></html>")
     properProgramSyntax

pageTests =
  [ testPCall
  , testProperProgram
  ]

pageTestsIO = do
  tests <- sequence pageTests
  return $ TestList tests