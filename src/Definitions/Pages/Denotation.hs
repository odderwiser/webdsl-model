module Definitions.Pages.Denotation where
import Actions.Effects 
import Definitions.Pages.Syntax
import Utils

type PageDefs eff eff' v = MLState PgName (PageDef (PEnv eff eff' v))

denoteDef :: (PageDefs eff eff' v <: f)
    =>  PageDef (PEnv eff eff' v) -> Free f ()
denoteDef pg@(PDef name vars e) = do
    (name :: PgName) <- ref pg
    return ()