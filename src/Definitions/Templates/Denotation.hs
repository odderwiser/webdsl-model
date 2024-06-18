module Definitions.Templates.Denotation where
import Actions.Effects
import Definitions.Templates.Syntax
import Utils
import Syntax

type TDefs eff eff' v = MLState (TName, [Type]) (TemplateDef (PEnv eff eff' v) (FreeEnv eff v))


denoteDefT :: (TDefs eff eff' v <: f)
    =>  TemplateDef (PEnv eff eff' v) (FreeEnv eff v) -> Free f ()
denoteDefT templ@(TDef name vars e) = do
    (name :: (TName, [Type])) <- ref templ
    return ()
