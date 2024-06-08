module Definitions.Program.Denotation where
import Definitions.Program.Syntax
import Utils
import Actions.Effects (MLState, ref)
import Definitions.Program.Effects (write, EnvType (..), GlobalScope)
import Definitions.Fun.Syntax (FDecl)
import Data.Bifunctor (Bifunctor(bimap))

foldProgram :: (Denote f eff v, Functor g)
    => Program (g (Fix f)) (Fix f) -> Program (g (FreeEnv eff v)) (FreeEnv eff v)
foldProgram (Fragment defs program) = Fragment (map (fmap foldD) defs) (foldD program)  


denoteDefs ::
  (FDecl <: g, GlobalScope g eff v <: eff')
  => EnvType -> [g (FreeEnv eff v)] -> Free eff' (Env eff v) -> Free eff' (Env eff v)
denoteDefs ty defs env = do 
  env' <- env 
  write ty defs env'
