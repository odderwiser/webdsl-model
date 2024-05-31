module FunTesta where

-- type Eff = MLState Address V + Cond + Abort V + End
-- type V =  Fix (LitBool + LitInt + Null + [])
-- type Module = Arith + Boolean + Eval + Fun + Stmt
-- type Out = Maybe (Either Bool Int)
-- type Envs = FDecl
-- type Eff' = FunctionEnv Eff V + End

-- runProgram :: Program (Envs (FreeEnv Eff V)) (FreeEnv Eff V)
--   -> Maybe (Either Bool Int)
-- runProgram (Fragment defs exp) = case unwrap
--   $ handle_ F.defsH (Env { varEnv = [], UEnv.defs =[]} :: Env Eff V ) 
--   $ denoteDefList defs of
--     (_, env) -> run exp env

-- testEqProgram id res syntax =  TestCase $
--   assertEqual id res $ runProgram $ foldProgram syntax

-- testfCall :: Test
-- testfCall = testEqProgram "simple function call"
--   (Just $ Right 7)
--   fCallSyntax

-- fCallSyntax :: Program (FDecl (Fix Module)) (Fix Module)
-- fCallSyntax = Fragment [FDecl "addThree" ["x"]
--     (injF $ OpArith Add (injVar "x") (injA 3))]
--   $ injF $ FCall "addThree" [injA 4]