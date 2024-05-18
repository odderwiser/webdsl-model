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

foldProgram :: (Denote f eff v, FDecl <: g, Def g)
    => Program (g (Fix f)) (Fix f) -> Program (g (FreeEnv eff v)) (FreeEnv eff v)
foldProgram (Fragment defs program) = Fragment (map foldDef defs) (foldD program)  

denoteDefs ::
  (FDecl <: g, GlobalScope g eff v <: eff')
  => EnvType -> [ g (FreeEnv eff v)] -> Free eff' (Env eff v) -> Free eff' (Env eff v)
denoteDefs ty defs env = do 
  env' <- env 
  write ty defs env'
-- denoteDefProgram :: Program (g (FreeEnv eff v)) (FreeEnv eff v) -> 
-- denoteDefProgram

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