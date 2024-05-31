module Program.Denotation where
import Program.Syntax
import Utils
import Actions.Effects (MLState, ref)
import Program.Effects (write, EnvType (..), GlobalScope)
import Actions.Syntax (FDecl)

foldProgram :: (Denote f eff v, Functor g)
    => Program (g (Fix f)) (Fix f) -> Program (g (FreeEnv eff v)) (FreeEnv eff v)
foldProgram (Fragment defs program) = Fragment (map (fmap foldD) defs) (foldD program)  

denoteDefs ::
  (FDecl <: g, GlobalScope g eff v <: eff')
  => EnvType -> [ g (FreeEnv eff v)] -> Free eff' (Env eff v) -> Free eff' (Env eff v)
denoteDefs ty defs env = do 
  env' <- env 
  write ty defs env'
