module Templates.Modules.Render.Denotation where
import Utils.Environment
import Templates.Modules.Render.Syntax as S
import Templates.Effects as E
import Utils hiding (denote)
import Actions.Str hiding (denote)
-- import qualified Actions.Modules.Str.Syntax as Str

denote :: (Functor eff, Functor eff', Functor v',
    Stream HtmlOut <: eff', v~ Fix v', LitStr <: v',
     E.Render v' <: eff')
  => S.Render (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
denote (XmlR xml) env env' lift = denoteXml xml env env' lift

denote (Output e) env env' lift = do
  v <-  lift $ e env
  e <- render v
  renderPlainText e True

denote (Raw e) env env' lift = do
  v <-  lift $ e env
  e <- render v
  renderPlainText e False


denoteXml :: (Functor eff, Functor eff',
    Stream HtmlOut <: eff', v~ Fix v', LitStr <: v')
  => Xml (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
denoteXml (Xml xml Nothing) env env' lift = do
    renderPlainText xml False

denoteXml (Xml xml (Just (exp, xml'))) env env' lift = do
    renderPlainText xml False
    exp <- lift $ exp env
    renderAttributeValue $ projS exp
    denoteXml xml' env env' lift