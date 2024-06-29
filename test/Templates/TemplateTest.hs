module Templates.TemplateTest where
import Templates.FrameworkIO as Tp
import Test.HUnit as T
import Utils.Denote
import Definitions.Templates.Framework
import Definitions.Program.Syntax
import Templates.Syntax as Ts
import Syntax as S
import Actions.Arith as A
import Actions.Syntax as A
import Actions.Str as As
import Templates.Modules.Lift.Denotation (consT)
import Definitions.Templates.Syntax (tDef)
import Utils
import Actions.FrameworkIO
import System.Directory (removeFile)

type Program'' = Program (Envs (PEnv (EffV V') (Eff' V') V) (EnvTy V')) ()
  (PEnv (EffV V') (Eff' V') V) 

testEqProgram :: String -> Out'
    -> Program DefSyntax () Module' -> IO T.Test
testEqProgram id res syntax = do
  let file = "./test/Templates/dbs/tps/"++id++ ".txt"
  output <- runProgram (foldTProgram syntax :: Program'') file
  return $ T.TestCase $
    T.assertEqual id res $ output
  
  --  TestCase $
  -- assertEqual id res $ runProgram $ foldTProgram syntax


defsSyn :: [DefSyntax]
defsSyn = [
    tDef "nestedVars" [("a", Int), ("b", S.String)] $
      tCall "inside" [(A.add (var "a") (int 1), Int)],
    tDef "inside" [("a", Int)] $ output $ var "a"
    ]

tCallSyntax :: Program DefSyntax () Module'
tCallSyntax = Fragment defsSyn  Nothing $ section False 
  $ tCall "nestedVars" [(int 5, Int), (As.str "a", S.String)]

testTCall = testEqProgram "test_TCall"
  (   "<html><head></head><body>"
     ++ "<span class=\"section section1\">"
     ++ "6</span></body></html>")
  tCallSyntax

elementsSyntax :: Program DefSyntax () Module'
elementsSyntax = Fragment 
  [ tDef "withElems" [] $ Ts.elements
  , tDef "callElems" [("a", Int)] $ tCallElems "withElems" [] $ output $ var "a"
  ] Nothing $ tCall "callElems" [(int 1, Int)]

testElems = testEqProgram "test Elems"
  (   "<html><head></head><body>"
    ++ "1</body></html>")
  elementsSyntax

elementsSyntaxCons :: Program DefSyntax () Module'
elementsSyntaxCons = Fragment 
  [ tDef "withElems" [] Ts.elements
  , tDef "callElems" [("a", Int)] $ tCallElems "withElems" [] $ consT (output $ var "a") (output $ var "a")
  ] Nothing $ tCall "callElems" [(int 1, Int)]

testElemsCons = testEqProgram "test Elems"
  (   "<html><head></head><body>"
    ++ "11</body></html>")
  elementsSyntaxCons

templateTests = do 
  tests <-  sequence 
    [ testTCall
    , testElems
    , testElemsCons
    ]
  return $ T.TestList tests
