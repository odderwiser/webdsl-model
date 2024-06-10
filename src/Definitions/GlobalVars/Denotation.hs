module Definitions.GlobalVars.Denotation where
import Actions.Effects (MLState, Random)
import Syntax
import Actions.Syntax
import Definitions.GlobalVars.Syntax
import Utils.Environment (FreeEnv)
import Utils
import Actions.Modules.Entity.Denotation (denoteEDecl)
import Actions.Modules.Str.Syntax (LitStr)

denoteDef :: forall eff v. (MLState Address (Fix v) <: eff,
    EntityDecl <: v, LitAddress <: v, Random String String <: eff,
    LitStr <: v, Null <: v)
    =>  GlobalVar (FreeEnv eff (Fix v)) -> Env eff (Fix v) -> Free eff ()
denoteDef (VDef name eDecl) env = do
    obj :: Fix v <- denoteEDecl eDecl env

    return ()