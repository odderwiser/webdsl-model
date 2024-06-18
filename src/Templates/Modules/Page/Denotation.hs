module Templates.Modules.Page.Denotation where
import Templates.Modules.Page.Syntax
import Actions.Effects (MLState, ref, assign)
import Definitions.Entity.Syntax (PName)
import Syntax (Type, Address)
import Utils
import Templates.Effects
import Text.HTML.TagSoup (Tag(TagClose, TagOpen))
import Actions.Handlers.Env (derefH, refH)
import Templates.Handlers.Env (templatesH, elementsH, pagesH)
import Definitions.Templates.Syntax (TemplateDef(TDef), TName, TBody (Body))
import Actions.Modules.Fun.Denotation (dropEnv, refVars, populateEnv)
import Definitions.Pages.Syntax
import Templates.Modules.Lift.Syntax (Weaken (Weaken))

denote ::forall eff eff' v. (Stream HtmlOut <: eff'
  , MLState Address v <: eff, Lift eff eff' v, Functor eff,
  MLState Address v <: eff', State Address <: eff') =>
    Page (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
-- this should do something with vars. DOesnt yet.
denote (PNavigate name vars text) pEnv = do
  renderTag $ TagOpen "a" [("href", name)]
  renderPlainText text False
  renderTag $ TagClose "a"

denote (TCall name atts args Nothing) env = do
  (body, env') <- populateTCall name args env
  body env { actionEnv = env'}

denote (TCall name atts args (Just elems)) env = do
  (loc, env')   <- refH (env, elems) elementsH env
  (body, env'') <- populateTCall name args env
  put loc
  body env' { actionEnv = env''}

denote Elements env = do
  loc <- get
  (env', elems) <- derefH loc elementsH env
  elems env'


populateTCall :: forall f eff' v. 
  (MLState Address v <: eff', MLState Address v <: f, Lift f eff' v) 
  => TName
  -> [(FreeEnv f v, Type)]
  -> TEnv f eff' v
  -> Free eff' (PEnv f eff' v, Env f v)
populateTCall name args env = do
  (TDef tName params body) :: TemplateDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefH (name, map snd args) templatesH env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  case body of (Body [Right body']) -> return (body', env')

denoteP :: (Stream HtmlOut <: eff'
  , MLState Address v <: eff, Lift eff eff' v, Functor eff
  , MLState Address v <: eff', State Address <: eff'
  , ReqParamsSt <: eff') =>
    PageCall (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteP (PCall name args params) env = do
  mapM_ assign params
  (PDef name params body) :: PageDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefH name pagesH env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  renderTag $ TagOpen "body" [("id", name)]
  isPageCall
  case body of
    Body [Right body'] -> body' env {actionEnv = env'}
  renderTag $ TagClose "body"

