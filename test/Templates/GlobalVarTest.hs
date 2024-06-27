{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Templates.GlobalVarTest where
import Definitions.Program.Syntax
import Definitions.GlobalVars.TemplatesFramework
import Utils
import Actions.FrameworkIO
import Test.HUnit
import Templates.Framework (Out')
import Syntax
import Definitions.Pages.Framework (pDefEnv)
import Definitions.Templates.Framework (tDefEnv)
import Templates.Syntax
import Actions.Arith as A
import Definitions.Pages.Syntax
import Actions.Syntax
import Definitions.Entity.Syntax ( EntityDef(EDef), eDef' )
import Definitions.GlobalVars.Syntax
import Definitions.Templates.Syntax (tDef)
import System.Directory (doesFileExist, removeFile)
import Actions.Handlers.Entity( DbStatus(..) )

testEqProgram :: ()
  => String -> Out' -> ProgramV (Fix Module) DefSyntax (BiFix T (Fix Module))
  ->  IO Test
testEqProgram id res syntax =  do
  let file = "./test/Actions/dbs/"++id++ ".txt"
  removeFile file
  (output, dbStatus) <- runObservableProgram (foldProgramVT syntax) file
  (output', dbStatus') <- runObservableProgram (foldProgramVT syntax) file
  return $ TestList
        [ TestCase $ assertEqual (id++" db read") (res, Empty) (output, dbStatus)
        , TestCase $ assertEqual (id++" db Write") (res, Success) (output', dbStatus') ]

defsSyn :: [DefSyntax]
defsSyn = [
    pDef "root" [] $  output $ pAccess (var "left") "a",
    tDef "inside" [("a", Int)]
      $ output $ pAccess (var "left") "a",
    eDef' "obj" [("a", Int)] []
    ]

pCallSyntax :: ProgramV (Fix Module) DefSyntax (BiFix T (Fix Module))
pCallSyntax = WithVars
    [ VDef "left" (EDecl "obj" [("a", int 1), ("other", var "right")])
    , VDef "right" (EDecl "obj" [("a", int 5), ("other", var "left")])
    ] $ Fragment defsSyn $ injBf $ pCallRoot

testPCall = testEqProgram "test1"
    (   "<html><head></head><body id=\"root\">"
     ++ "1</body></html>")
     pCallSyntax

globalVarTests = do
    test1 <- testPCall
    return $ TestList
        [ test1
        ]