module Definitions.Fun.Denotation where
import Actions.Effects
import Definitions.Fun.Syntax
import Utils
-- import Definitions.Program.Effects (GlobalScope, EnvType (Defs))
-- import qualified Definitions.Program.Denotation as P

denoteDef :: (MLState FunName (Function eff v) <: eff') 
  => FDecl (FreeEnv eff v) -> Free eff' ()
denoteDef decl@(FDecl name _ _) = do
  (name :: FunName) <- ref decl
  return ()

-- denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
--   => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
-- denoteDefs defs = P.denoteDefs Defs defs $ Pure $ Env {} 