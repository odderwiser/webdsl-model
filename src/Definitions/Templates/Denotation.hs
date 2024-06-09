module Definitions.Templates.Denotation where
import Actions.Effects
import Definitions.Templates.Syntax
import Utils
import Syntax

type PageDefs eff eff' v = MLState PgName (PageDef (PEnv eff eff' v))
type TDefs eff eff' v = MLState (TName, [Type]) (TemplateDef (PEnv eff eff' v))

denoteDef :: (PageDefs eff eff' v <: f)
    =>  PageDef (PEnv eff eff' v) -> Free f ()
denoteDef pg@(PDef name vars e) = do
    (name :: PgName) <- ref pg
    return ()


denoteDefT :: (TDefs eff eff' v <: f)
    =>  TemplateDef (PEnv eff eff' v) -> Free f ()
denoteDefT templ@(TDef name vars e) = do
    (name :: (TName, [Type])) <- ref templ
    return ()