module Definitions.GlobalVars.TemplatesFramework where
import Utils
import Data.Bifunctor
import Definitions.Program.Syntax
import Definitions.Templates.Framework
import Definitions.GlobalVars.Syntax

foldProgramVT :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Functor g, VarListT <:: f)
    => ProgramV (Fix h) (g (Fix h) \/ g (BiFix f (Fix h))) (BiFix f (Fix h))
    -> Program (g (FreeEnv eff v \/ PEnv eff eff' v)) (PEnv eff eff' v)
foldProgramVT (WithVars vars (Fragment defs program)) = foldTProgram 
  (Fragment defs (injBf $ VList vars program)) 