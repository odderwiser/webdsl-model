module Program.Denotation where
import Program.Syntax
import Utils.Denote
import Eval.Effects (MLState, ref)
import Utils.Composition
import Utils.Free
import Utils.Environment 
import Utils.Handler
import Program.Effects (write, EnvType (..), GlobalScope)
import Utils.Fix
import Fun.Syntax (FDecl)

foldProgram :: (Denote f2 eff2 v2, FDecl <: g, Def g, Denote f1 eff1 v1)
    => Program (g (Fix f1)) (Fix f2) -> Program (g (FreeEnv eff1 v1)) (FreeEnv eff2 v2)
foldProgram (Fragment defs program) = Fragment (map foldDef defs) (foldD program)  

-- denoteProgram :: (Functor eff, GlobalScope () <: g)
--   => Program (FreeEnv eff v) (FreeEnv eff v)
--   -> FreeEnv eff v
-- denoteProgram (Fragment decls program) env = do
--   env' <- write Defs decls env
--   return program env'


-- runProgram (Fragment decls program) handler = 
--     case unwrap $ handle_ handler (Env {}) $ decls of
--         (env') -> unwrap $ program env'  

-- denoteDecls decls = do mapM ref declsdenoteProgram :: (Functor eff, MLState Name (FreeEnv eff v) <: g)
--   => Program (FreeEnv eff v) (FreeEnv eff v)
--   -> FreeEnv eff v
-- denoteProgram (Fragment decls program) env =  do