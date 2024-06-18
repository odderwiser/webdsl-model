module Templates.TemplateTest where
import Templates.Framework as Tp
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
    tDef "nestedVars" [("a", Int), ("b", S.String)] 
      [Right $ tCall "inside" [(A.add (var "a") (int 1), Int)]],
    tDef "inside" [("a", Int)] 
      [ Right $ output $ var "a" ] 
    ]

tCallSyntax :: Program DefSyntax Module'
tCallSyntax = Fragment defsSyn $ section False 
  $ tCall "nestedVars" [(int 5, Int), (As.str "a", S.String)]

testTCall = testEqProgram "test TCall"
  (   "<html><head></head><body>"
     ++ "<span class=\"section section1\">"
     ++ "6</span></body></html>")
  tCallSyntax

elementsSyntax :: Program DefSyntax Module'
elementsSyntax = Fragment 
  [ tDef "withElems" [] [Right Ts.elements]
  , tDef "callElems" [("a", Int)] [Right $ tCallElems "withElems" [] $ output $ var "a" ] 
  ] $ tCall "callElems" [(int 1, Int)]

testElems = testEqProgram "test Elems"
  (   "<html><head></head><body>"
    ++ "1</body></html>")
  elementsSyntax

elementsSyntaxCons :: Program DefSyntax Module'
elementsSyntaxCons = Fragment 
  [ tDef "withElems" [] [ Right $ Ts.elements ]
  , tDef "callElems" [("a", Int)] $ [ Right $ tCallElems "withElems" [] $ consT (output $ var "a") (output $ var "a") ]
  ] $ tCall "callElems" [((int 1), Int)]

testElemsCons = testEqProgram "test Elems"
  (   "<html><head></head><body>"
    ++ "11</body></html>")
  elementsSyntaxCons

templateTests = T.TestList 
  [ testTCall
  , testElems
  , testElemsCons
  ]
