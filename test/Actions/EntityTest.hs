-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Actions.EntityTest where
import Test.HUnit
import Utils as U
import Actions.Framework
import Definitions.Program.Syntax
import Definitions.Entity.Framework
import Definitions.Entity.Syntax
import Syntax
import Definitions.Program.Denotation
import Actions.Syntax
import Actions.Arith as A
import Definitions.Fun.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Definitions.GlobalVars.Syntax (Uuid)
import Actions.Modules.Entity.Denotation (mapProperties)

testEq :: Denote m Eff V
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

testEqProgram :: ()
  => String -> Out -> Program (Envs (Fix Module)) (Fix Module) -> Test
testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldProgram syntax

--------------------------------

eDeclSyntax :: Program (Envs (Fix Module)) (Fix Module)
eDeclSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id] []]
  $ eDecl "dummy1" [("y", int 1)]

testEDecl = testEqProgram 
  "testing declaration"
  (injF $ Box "12e8d002-5511-3b76-9813-b6c2bf959bfe") eDeclSyntax 
  
actualUnitTestSyntax = mapProperties 
  (EDecl "dummy1" [("y", int 1)] :: EntityDecl (Fix Module)) 
  [lit 1 :: Out] [("id", box "some")]  

testMapProperties = TestCase $ assertEqual "testMapProperties"
  (EDecl "dummy1" [("id", box "some"), ("y", lit 1)]) actualUnitTestSyntax

testGetProperty :: Test
testGetProperty = testEqProgram "simple function call"
  (A.lit 1)
  propSyntax

propSyntax :: Program (Envs (Fix Module)) (Fix Module)
propSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id] []]
  $ varInit "dummy"
    (eDecl "dummy1" [("y", int 1)])
    (pAccess (var "dummy") "y")

testMethodCall = testEqProgram "simple function call"
  (A.lit 6)
  objFunSyntax

objFunSyntax :: Program (Envs (Fix Module)) (Fix Module)
objFunSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id]
    [inj $ FDecl "dummy2" [ "z" ]
      $ return' $ add (var "z") (pVar "y")]]
  $ varInit "dummy"
    (eDecl "dummy1" [("y", int 1)])
    (eCall (var "dummy") "dummy2" [int 5] )

testDefsStoring :: Test
testDefsStoring = TestCase $
  assertEqual "storing the def"
  ("dummy1", [("x", Int), ("y", Int)])
  (case
  handle_ entityDefsH Env{entityDefs = []}
    $ (denoteDefList :: [Envs (FreeEnv Eff V)] -> Free Eff' [()]) (map (fmap foldD) dummy1Definition) of
      (Pure (_, env)) ->
        case U.entityDefs env of
        [EDef name params _ _] -> (name, params))

dummy1Definition :: [Envs (Fix Module)]
dummy1Definition = [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id] []]

entityTests :: Test
entityTests = TestList
  [ testDefsStoring
  , testMapProperties
  , testEDecl
  , testGetProperty
  , testMethodCall
  ]
