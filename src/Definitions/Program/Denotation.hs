module Definitions.Program.Denotation where
import Definitions.Program.Syntax
import Utils
import Actions.Effects (MLState, ref)
import Definitions.Program.Effects (write, EnvType (..), GlobalScope)
import Definitions.Fun.Syntax (FDecl)
import Data.Bifunctor (Bifunctor(bimap))
import Definitions.GlobalVars.Syntax
import Templates.Modules.Lift.Syntax

foldProgram :: forall f g eff v. (Denote f eff v, Functor g)
    => Program (g (Fix f)) (Fix f) -> Program (g (FreeEnv eff v)) (FreeEnv eff v)
foldProgram (Fragment defs program) = Fragment (map (fmap foldD) defs) (foldD program)

foldProgramV :: forall f g eff v. (Denote f eff v, Functor g, VarList <: f)
    => ProgramV (Fix f) (g (Fix f)) (Fix f) -> Program (g (FreeEnv eff v)) (FreeEnv eff v)
foldProgramV (WithVars vars (Fragment defs program)) = foldProgram 
  (Fragment defs (injF $ Weaken $ VList vars program)) 


denoteDefs ::
  (FDecl <: g, GlobalScope g eff v <: eff')
  => EnvType -> [g (FreeEnv eff v)] -> Free eff' (Env eff v) -> Free eff' (Env eff v)
denoteDefs ty defs env = do
  env' <- env
  write ty defs env'
