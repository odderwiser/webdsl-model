module Render.Denotation where
import Utils.Environment
import Render.Syntax
import Layout.Effects (renderString, RenderHtml)
import Utils.Composition
import Utils.Free

denote :: forall eff eff' v. (Functor eff, Functor eff',
    RenderHtml <: eff')
  => Xml (FreeEnv eff v) (PEnv eff eff' v)
  -> Env eff v -> TEnv eff eff' v 
  -> (Free eff v -> Free eff' v) 
  -> Free eff' ()
denote (Xml xml Nothing) env env' lift = do
    renderString xml

denote (Xml xml (Just (exp, xml'))) env env' lift = do
    renderString xml
    exp <- lift $ exp env
    denote xml' env env' lift