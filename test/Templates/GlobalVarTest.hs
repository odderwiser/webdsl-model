module Templates.GlobalVarTest where
import Definitions.Program.Syntax
import Definitions.GlobalVars.TemplatesFramework
import Utils
import Actions.Framework
import Test.HUnit
import Templates.Framework (Out')

testEqProgram :: ()
  => String -> Out' -> ProgramV (Fix Module) DefSyntax (BiFix T (Fix Module)) 
  ->  IO Test
testEqProgram id res syntax =  do
    program <- runProgram (foldProgramVT syntax) ("./test/Actions/dbs/"++id++ ".txt")
    return $ TestCase $
        assertEqual id res program