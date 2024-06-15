module Actions.GlobalVarTest where
import Utils
import Definitions.GlobalVars.ActionsFramework as G
import Actions.Framework
import Test.HUnit
import Definitions.Program.Syntax
import Definitions.Program.Denotation (foldProgramV)
import Definitions.GlobalVars.Syntax
import Actions.Syntax (EntityDecl(EDecl), pAccess)
import Definitions.Entity.Syntax
import Syntax
import Actions.Arith
import Actions.Syntax
import Actions.Values
import Definitions.Fun.Syntax (FDecl(FDecl))

-- testEq :: Denote m EffA V
--   => String -> Out -> Fix m -> FilePath ->  IO Test
-- testEq id res syntax filePath = do
--     syntax' <-  G.runExp (foldD syntax) filePath
--     return $ TestCase $
--         assertEqual id res syntax'

testEqProgram :: ()
  => String -> Out -> ProgramV (Envs (Fix Sym)) (Fix Sym) ->  IO Test
testEqProgram id res syntax =  do
    (program, heap) <- runProgram (foldProgramV syntax) ("./test/Actions/dbs/"++id++ ".txt")
    return $ TestCase $
        assertEqual id res program

--- this seem to be working ??? 

variableTest = testEqProgram "test1" (boxI 1) variableSyntax


variableSyntax :: ProgramV (Envs (Fix Sym)) (Fix Sym)
variableSyntax = WithVars [VDef "test" (EDecl "obj" [("a", int 1)]) ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int)] [Id] [] ]
      (pAccess (var "test") "a")
    )

fCallTest = testEqProgram "test2" (boxI 2) fCallSyntax

fCallSyntax :: ProgramV (Envs (Fix Sym)) (Fix Sym)
fCallSyntax = WithVars [VDef "test" (EDecl "obj" [("a", int 1)]) ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int)] [Id] [FDecl "doSomething" [] (int 2)] ]
      (eCall (var "test") "doSomething" [])
    )

globalVarTests = do
    test1 <- variableTest
    test2 <- fCallTest
    return $ TestList 
        [ test1
        , test2
        ]