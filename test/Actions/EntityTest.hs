-- {-# LANGUAGE ScopedTypeVariables #-}
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

testEq :: Denote m Eff V
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

testEqProgram :: ()
  => String -> Out -> Program (Envs (Fix Module)) (Fix Module) -> Test
testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldProgram syntax

--------------------------------


testGetProperty :: Test
testGetProperty = testEqProgram "simple function call"
  (A.lit 1)
  propSyntax

propSyntax :: Program (Envs (Fix Module)) (Fix Module)
propSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] []]
  $ varInit "dummy" 
    (eDecl "dummy1" [("y", int 1)]) 
    (pAccess (var "dummy") "y")

testMethodCall = testEqProgram "simple function call"
  (A.lit 6)
  objFunSyntax

objFunSyntax :: Program (Envs (Fix Module)) (Fix Module)
objFunSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] 
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
        [EDef name params funs] -> (name, params))

dummy1Definition :: [Envs (Fix Module)]
dummy1Definition = [inj $ EDef "dummy1" [("x", Int), ("y", Int)] []]

entityTests :: Test
entityTests = TestList
  [ testDefsStoring
  , testGetProperty
  , testMethodCall
  ]