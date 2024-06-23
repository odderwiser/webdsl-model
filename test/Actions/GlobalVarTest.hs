{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Actions.GlobalVarTest where
import Utils
import Definitions.GlobalVars.ActionsFramework as G
import Actions.Framework
import Test.HUnit
import Definitions.Program.Syntax
import Definitions.Program.Denotation (foldProgramV)
import Definitions.GlobalVars.Syntax
import Definitions.Entity.Syntax
import Syntax
import Actions.Arith
import Actions.Syntax
import Actions.Values
import Definitions.Fun.Syntax (FDecl(FDecl))
import System.Directory (doesFileExist, removeFile)
import Actions.Handlers.Entity (DbStatus(..))

-- testEq :: Denote m EffA V
--   => String -> Out -> Fix m -> FilePath ->  IO Test
-- testEq id res syntax filePath = do
--     syntax' <-  G.runExp (foldD syntax) filePath
--     return $ TestCase $
--         assertEqual id res syntax'

type ProgramA e f = ProgramV f e f

testEqProgram :: ()
  => String -> Out -> ProgramA (Envs (Fix Sym)) (Fix Sym) ->  IO Test
testEqProgram id res syntax =  do
  let file = "./test/Actions/dbs/"++id++ ".txt"
  removeFile file
  ((output, heap), dbStatus) <- runProgram (foldProgramV syntax) file
  ((output', heap'), dbStatus') <- runProgram (foldProgramV syntax) file
  return $ TestList
    [ TestCase $ assertEqual (id++" db read") (res, Empty) (output, dbStatus)
    , TestCase $ assertEqual (id++" db Write") (res, Success) (output', dbStatus') ]
--- this seem to be working ??? 

variableTest = testEqProgram "test1" (boxI 1) variableSyntax


variableSyntax :: ProgramA (Envs (Fix Sym)) (Fix Sym)
variableSyntax = WithVars [VDef "test" (EDecl "obj" [("a", int 1)]) ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int)] [Id] [] ]
      (pAccess (var "test") "a")
    )

fCallTest = testEqProgram "test2" (boxI 2) fCallSyntax

fCallSyntax :: ProgramA (Envs (Fix Sym)) (Fix Sym)
fCallSyntax = WithVars [VDef "test" (EDecl "obj" [("a", int 1)]) ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int)] [Id] [FDecl "doSomething" [] (int 2)] ]
      (eCall (var "test") "doSomething" [])
    )

objectFieldTest = testEqProgram "test3" (boxI 1) objectFieldSyntax

objectFieldSyntax :: ProgramA (Envs (Fix Sym)) (Fix Sym)
objectFieldSyntax = WithVars [VDef "test" (EDecl "obj" [("a", int 1), ("parent", var "test")]) ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int), ("parent", Entity "obj")] [Id] [FDecl "doSomething" [] (int 2)] ]
      (pAccess (var "test") "a")
    )

objectFieldTest' = testEqProgram "test4" (boxI 6) objectFieldSyntax'

objectFieldSyntax' :: ProgramA (Envs (Fix Sym)) (Fix Sym)
objectFieldSyntax' = WithVars [VDef "test" (EDecl "obj" [("a", int 1), ("parent", var "test")]) ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int), ("parent", Entity "obj")] [Id] [FDecl "doSomething" [] (int 2)] ]
      (add (pAccess (pAccess (var "test") "parent") "a") (int 5))
    )

twoObjectsTest = testEqProgram "test5" (boxI 17) twoObjectsSyntax

twoObjectsSyntax :: ProgramA  (Envs (Fix Sym)) (Fix Sym)
twoObjectsSyntax = WithVars
    [ VDef "left" (EDecl "obj" [("a", int 1), ("other", var "right")])
    , VDef "right" (EDecl "obj" [("a", int 5), ("other", var "left")])
    ]
    ( Fragment
      [inj $ EDef "obj" [("a", Int), ("other", Entity "obj")] [Id] [FDecl "doSomething" [] (int 2)] ]
      (add (multiply (pAccess (pAccess (var "left") "other") "a") (int 3))
        (multiply (pAccess (pAccess (var "right") "other") "a") (int 2))
      )
    )

globalVarTests = do
    test1 <- variableTest
    test2 <- fCallTest
    test3 <- objectFieldTest
    test4 <- objectFieldTest'
    test5 <- twoObjectsTest
    return $ TestList
        [ test1
        , test2
        , test3
        , test4
        , test5
        ]