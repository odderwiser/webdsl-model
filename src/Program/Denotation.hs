module Program.Denotation where
import Program.Syntax
import Utils.Denote
import Eval.Effects (MLState, ref)
import Utils.Composition
import Utils.Free

denoteProgram :: (Functor eff, MLState (Name, envType) (FreeEnv eff v) <: g)
  => Program (Free g v') (FreeEnv eff v)
  -> FreeEnv eff v
denoteProgram (Fragment decls program) env = do
    () <- mapM ref decls
    program env'

runProgram (Fragment decls program) = 
    case unwrap $ handle_ handler (Env {}) $ decls of
        (env') -> unwrap $ program env'  

denoteDecls decls = do mapM ref decls