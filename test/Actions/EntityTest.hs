-- {-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE ScopedTypeVariables #-}
module Actions.EntityTest where
import Test.HUnit
import Utils as U
import Actions.FrameworkIO
import Definitions.Program.Syntax
import Definitions.Entity.Framework
import Definitions.Entity.Syntax
import Syntax
import Definitions.Program.Denotation
import Actions.Syntax
import Actions.Arith as A
import Definitions.Fun.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Actions.Modules.Entity.Denotation (mapProperties)
import Actions.Values
import System.Directory (removeFile)
import Actions.Handlers.Entity (DbStatus(..))

-- testEq :: Denote m Eff V
--   => String -> Out -> Fix m -> IO Test
-- testEq id res syntax = do
--   let file = "./test/Actions/dbs/entity/"++id++ ".txt"
--   removeFile file
--   (output, dbStatus) <- runExp (foldD syntax) file
--   return 
--     $ TestCase  
--     $ assertEqual id res output

testEqProgram :: ()
  => String -> Out -> Program (Envs (Fix Module)) () (Fix Module) -> IO Test
testEqProgram id res syntax =  do
  let file = "./test/Actions/dbs/entity/"++id++ ".txt"
  -- removeFile file
  (output, dbStatus) <- runProgram (foldProgram syntax) file
  return $ TestCase 
    $ assertEqual (id++" db read") (res) (output)

--------------------------------

eDeclSyntax :: Program (Envs (Fix Module)) () (Fix Module)
eDeclSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id] [] []] Nothing
  $ eDecl "dummy1" [("y", int 1)]

testEDecl :: IO Test
testEDecl = do 
  let file = "./test/Actions/dbs/entity/tDecl.txt"
  -- removeFile file
  (uuid, dbstatus) <- runProgram (foldProgram eDeclSyntax) file
  return $ TestCase $
    assertEqual "testing_declaration" 36 $ length (unbox' $ uuid :: Uuid)
  
  -- testEqProgram 
  -- "testing declaration"
  -- (box "8427aa4f-128f-3c25-a479-9784d401123c") eDeclSyntax 
  
actualUnitTestSyntax = mapProperties 
  (EDecl "dummy1" [("y", int 1)] :: EntityDecl (Fix Module)) 
  [boxI 1 :: Out] [("id", box "some")]  

testMapProperties = TestCase $ assertEqual "testMapProperties"
  (EDecl "dummy1" [("id", box "some"), ("y", boxI 1)]) actualUnitTestSyntax

testGetProperty :: IO Test
testGetProperty = testEqProgram "simple _function_call"
  (boxI 1)
  propSyntax

propSyntax :: Program (Envs (Fix Module)) () (Fix Module)
propSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id] [] []] Nothing
  $ varInit "dummy"
    (eDecl "dummy1" [("y", int 1)])
    (pAccess (var "dummy") "y")

testMethodCall = testEqProgram "method_call"
  (boxI 6)
  objFunSyntax

objFunSyntax :: Program (Envs (Fix Module)) () (Fix Module)
objFunSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id]
    [inj $ FDecl "dummy2" [ "z" ] 
      $ return' $ add (var "z") (pVar "y")] []] Nothing
  $ varInit "dummy"
    (eDecl "dummy1" [("y", int 1)])
    (eCall (var "dummy") "dummy2" [int 5] )

testDefsStoring :: Test
testDefsStoring = TestCase $
  assertEqual "storing_the_def"
  ("dummy1", [("x", Int), ("y", Int)])
  (case
  handle_ entityDefsH Env{entityDefs = []}
    $ (denoteDefList :: [Envs (FreeEnv Eff V)] -> Free (Eff' Eff V') [()]) (map (fmap foldD) dummy1Definition) of
      (Pure (_, env)) ->
        case U.entityDefs env of
        [EDef name params _ _ _] -> (name, params))

dummy1Definition :: [Envs (Fix Module)]
dummy1Definition = [inj $ EDef "dummy1" [("x", Int), ("y", Int)] [Id] [] []]

entityTests :: IO Test
entityTests = do
  testEDecl' <- testEDecl
  testMethodCall' <- testMethodCall
  testGetProperty' <- testGetProperty
  return $ TestList
    [ testDefsStoring
    , testMapProperties
    , testEDecl'
    , testGetProperty'
    , testMethodCall'
    ]
