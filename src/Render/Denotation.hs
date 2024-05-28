module Render.Denotation where
import Utils.Environment
import Render.Syntax
import Layout.Effects
import Utils.Composition
import Utils.Free
import Str.Syntax
import Utils.Fix

denote :: (Functor eff, Functor eff',
    RenderHtml <: eff', v~ Fix v', LitStr <: v')
  => Xml (FreeEnv eff v) (PEnv eff eff' v)
  -> Env eff v -> TEnv eff eff' v 
  -> (Free eff v -> Free eff' v) 
  -> Free eff' ()
denote (Xml xml Nothing) env env' lift = do
    renderPlainText xml

denote (Xml xml (Just (exp, xml'))) env env' lift = do
    renderPlainText xml
    exp <- lift $ exp env
    renderString $ projS exp
    denote xml' env env' lift