module Definitions.Entity.Denotation where
import Actions.Effects
import Definitions.Entity.Syntax
import Utils

type EntityDefsEnv eff v = (MLState EName (EntityDef (FreeEnv eff v)))

denoteDef :: (EntityDefsEnv eff v <: f) 
  => EntityDef (FreeEnv eff v) -> Free f ()
denoteDef entity@(EDef name _ _ _ _) = do
  (name :: EName) <- ref entity 
  return ()  