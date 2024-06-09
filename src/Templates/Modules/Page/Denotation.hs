module Templates.Modules.Page.Denotation where
import Templates.Modules.Page.Syntax
import Actions.Effects (MLState, ref)
import Definitions.Entity.Syntax (PName)
import Syntax (Type, Address)
import Utils
import Templates.Effects
import Text.HTML.TagSoup (Tag(TagClose, TagOpen))
import Actions.Handlers.Env (derefH, refH)
import Templates.Handlers.Env (templatesH, elementsH)
import Definitions.Templates.Syntax (TemplateDef(TDef))
import Actions.Modules.Fun.Denotation (dropEnv, refVars, populateEnv)

denote ::forall eff eff' v. (Stream HtmlOut <: eff'
  , MLState Address v <: eff, Lift eff eff' v, Functor eff,
  MLState Address v <: eff', State Address <: eff') =>
    Page (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
-- this should do something with vars. DOesnt yet.
denote (PNavigate name vars text) pEnv = do
  renderTag $ TagOpen "a" [("href", name)]
  renderPlainText text False
  renderTag $ TagClose "a"

denote (TCall name atts args Nothing) env = do
  (TDef tName params body) :: TemplateDef (PEnv eff eff' v) 
    <- derefH (name, map snd args) templatesH env 
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  body $ env {actionEnv = env'}

denote (TCall name atts args (Just elems)) env = do
  (loc, env') <- refH (env,elems) elementsH env
  (TDef tName params body) :: TemplateDef (PEnv eff eff' v) 
    <- derefH (name, map snd args) templatesH env 
  env''         <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  put loc 
  body env' {actionEnv = env''}

denote Elements env = do
  loc <- get 
  (env', elems) <- derefH loc elementsH env
  elems env'