module Page.Denotation where
import Page.Syntax
import Utils.Environment (FreeEnv, Env)
import Actions.Effects (MLState, ref)
import Utils.Free
import Utils.Composition
import Entity.Syntax (PName)
import Syntax (Type)
import Utils.Fix
import Layout.Effects
import qualified Attributes.Denotation as A
import qualified Actions.Modules.Fun.Denotation as F

type PageDefs eff v = MLState PgName (PageDef (Env eff v -> Free eff ()))
type TDefs eff v = MLState (TName, [Type]) (Env eff v -> Free eff ())

denoteDef :: (PageDefs eff v <: f, TDefs eff v <: f)
    =>  PageDef (Env eff v -> Free eff ()) -> Free f ()
denoteDef pg@(PDef name vars e) = do
    (name :: PgName) <- ref pg
    return ()

denoteDefT templ@(TDef name vars e) = do
    (name :: (TName, [Type])) <- ref templ
    return ()

denote :: (RenderHtml <: eff) =>
    Page (FreeEnv eff (Fix v))
  -> Env eff (Fix v) -> Free eff ()
-- this should do something with vars. DOesnt yet.
denote (PNavigate name vars text) env = do
    renderLink name
    renderPlainText text
    renderEndTag "a"

-- denote (TCall name atts vars body elem) env = do
--     (TDef name vars' e) <- derefH _ (name, map snd vars)
--     env' <- F.populateEnv env (map fst vars') (map fst vars)
--     elem <- elem env
--     A.denote atts env
--     body env
--     return ()
    
