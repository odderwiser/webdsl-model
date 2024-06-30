module Templates.Modules.Render.Denotation where
import Utils.Environment
import Templates.Modules.Render.Syntax as S
import Templates.Effects as E
import Utils hiding (denote)
import Actions.Str hiding (denote)
import Actions.Values
-- import qualified Actions.Modules.Str.Syntax as Str

denote :: (Functor eff, Functor eff', Functor v',
    Stream HtmlOut <: eff', v~ Fix v', LitStr <: v',
     E.Render v <: eff', Lift eff eff' v)
  => S.Render (PEnv eff eff' v) (FreeEnv eff v) 
  -> PEnv eff eff' v
denote (XmlR xml) env = denoteXml xml env

denote (Output e) env = do
  v <-  lift $ e (actionEnv env)
  e <- render v 
  renderPlainText e True

denote (Raw e) env  = do
  v <-  lift $ e (actionEnv env)
  e <- render v
  renderPlainText e False


denoteXml :: (Functor eff, Functor eff',
    Stream HtmlOut <: eff', v~ Fix v', LitStr <: v', Lift eff eff' v)
  => Xml (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteXml (Xml xml Nothing) env = do
    renderPlainText xml False

denoteXml (Xml xml (Just (exp, xml'))) env= do
    renderPlainText xml False
    exp <- lift $ exp (actionEnv env)
    renderAttributeValue $ (unbox exp :: String)
    denoteXml xml' env 

denoteProcess :: Functor eff' 
  => S.Render (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteProcess _ env = return ()