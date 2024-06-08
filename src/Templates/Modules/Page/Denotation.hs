module Templates.Modules.Page.Denotation where
import Templates.Modules.Page.Syntax
import Actions.Effects (MLState, ref)
import Definitions.Entity.Syntax (PName)
import Syntax (Type, Address)
import Utils
import Templates.Effects
import Text.HTML.TagSoup (Tag(TagClose, TagOpen))
import Actions.Handlers.Env (derefH)
import Templates.Handlers.Env (templatesH)
import Definitions.Templates.Syntax (TemplateDef(TDef))
import Actions.Modules.Fun.Denotation (dropEnv, refVars, populateEnv)

denote ::forall eff eff' v. (Stream HtmlOut <: eff'
  , MLState Address v <: eff, Lift eff eff' v, Functor eff,
  MLState Address v <: eff') =>
    Page (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
-- this should do something with vars. DOesnt yet.
denote (PNavigate name vars text) pEnv = do
  renderTag $ TagOpen "a" [("href", name)]
  renderPlainText text False
  renderTag $ TagClose "a"

denote (TCall name atts args elements) env = do
  (TDef tName params body) :: TemplateDef (PEnv eff eff' v) 
    <- derefH (name, map snd args) templatesH env 
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  body $ env {actionEnv = env'}
