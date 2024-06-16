module Templates.GlobalVarTest where
import Definitions.Program.Syntax
import Definitions.GlobalVars.TemplatesFramework
import Utils
import Actions.Framework
import Test.HUnit
import Templates.Framework (Out')
import Syntax
import Definitions.Pages.Framework (pDefEnv)
import Definitions.Templates.Framework (tDefEnv)
import Templates.Syntax
import Actions.Arith as A
import Definitions.Pages.Syntax
import Actions.Syntax
import Definitions.Entity.Syntax (EntityDef(EDef))
import Definitions.GlobalVars.Syntax

testEqProgram :: ()
  => String -> Out' -> ProgramV (Fix Module) DefSyntax (BiFix T (Fix Module)) 
  ->  IO Test
testEqProgram id res syntax =  do
    program <- runProgram (foldProgramVT syntax) ("./test/Actions/dbsT/"++id++ ".txt")
    return $ TestCase $
        assertEqual id res program

defsSyn :: [DefSyntax]
defsSyn = [
    pDefEnv "root" [] 
      $ output $ pAccess (var "left") "a",
    tDefEnv "inside" [("a", Int)] 
      $ output $ pAccess (var "left") "a",
    eDefEnv "obj" [("a", Int)] []
    ]

pCallSyntax :: ProgramV (Fix Module) DefSyntax (BiFix T (Fix Module))
pCallSyntax = WithVars 
    [ VDef "left" (EDecl "obj" [("a", int 1), ("other", var "right")]) 
    , VDef "right" (EDecl "obj" [("a", int 5), ("other", var "left")])
    ] $ Fragment defsSyn $ injBf $ PCall "root" []

testPCall = testEqProgram "test1"
    (   "<html><head></head><body>"
     ++ "1</body></html>")
     pCallSyntax

globalVarTests = do
    test1 <- testPCall
    return $ TestList
        [ test1
        ]