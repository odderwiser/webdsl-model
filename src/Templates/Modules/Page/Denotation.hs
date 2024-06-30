module Templates.Modules.Page.Denotation where
import Templates.Modules.Page.Syntax
import Actions.Effects (MLState, ref, assign, write, Random, Writer)
import Definitions.Entity.Syntax (PName)
import Syntax (Type, Address)
import Utils
import Templates.Effects
import Text.HTML.TagSoup (Tag(TagClose, TagOpen))
import Actions.Handlers.Env (derefH, refH)
import Templates.Handlers.Env (templatesH, elementsH, pagesH)
import Definitions.Templates.Syntax (TemplateDef(TDef), TName, TBody (Body), StatementType (..))
import Actions.Modules.Fun.Denotation (dropEnv, refVars, populateEnv, refVar)
import Definitions.Pages.Syntax
import Templates.Modules.Lift.Syntax (Weaken (Weaken))
import Control.Monad (foldM)
import Data.Either (lefts, rights)
import Templates.Modules.Forms.Syntax
import Actions.Values as V
import Actions.Handlers.Heap (environment)
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Syntax (VName)
import Actions.Modules.Eval.Denotation (derefEnv')
import Data.Bifunctor (second)
import GHC.Float (timesDouble)
import Definitions.Syntax (Uuid)

denote ::forall eff eff' v v'.
  ( Stream HtmlOut <: eff' , MLState Address v <: eff
  , Lift eff eff' v, MLState Address v <: eff'
  , State Address <: eff', Random Label String <: eff'
  , Reader () (Maybe TId) <: eff', Writer TId <: eff'
  , State TSeed <: eff'
  , v ~ Fix v', Null <: v'
  ) => Page (PEnv eff eff' v) (FreeEnv eff v)
    -> PEnv eff eff' v
-- this should do something with vars. DOesnt yet.
denote (PNavigate name vars text) pEnv = do
  renderTag $ TagOpen "a" [("href", name)]
  renderPlainText text False
  renderTag $ TagClose "a"

denote (TCall name atts args Nothing) env = do
  (body, env') <- populateTCall name args env
  tEnv <- storeTemplateId name env
  body tEnv { actionEnv = env'}

denote (TCall name atts args (Just elems)) env = do
  (loc, env')   <- refElements env elems
  (body, env'') <- populateTCall name args env'
  put loc
  tEnv <- storeTemplateId name env'
  body tEnv { actionEnv = env''}

denote Elements env = do
  loc <- get
  (env', elems) <- derefElements env loc
  elems env'

refElements env elems = refH (env, elems) elementsH env
derefElements env loc = derefH loc elementsH env
populateTCall :: forall f eff' v.
  (MLState Address v <: eff', MLState Address v <: f
  , Lift f eff' v)
  => TName
  -> [(FreeEnv f v, Type)] -> TEnv f eff' v
  -> Free eff' (PEnv f eff' v, Env f v)
populateTCall name args env = do
  (TDef tName params body) :: TemplateDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefH (name, map snd args) templatesH env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  return (body, env')

denoteBody :: (Heap v <: eff', Null <: v
  , Lift eff eff' (Fix v)
  ) => TBody (PEnv eff eff' (Fix v)) (FreeEnv eff (Fix v))
  -> PEnv eff eff' (Fix v)
denoteBody (Body names defs stmts) env = do
  env'  <- foldM refName env names
  defs env'
  stmts env'

denoteE :: (Heap v <: eff', Functor eff
  , Lift eff eff' (Fix v))
  => EvalT (PEnv eff eff' (Fix v)) (FreeEnv eff (Fix v))
  -> PEnv eff eff' (Fix v)
denoteE (VarDeclT name) env   = return ()
denoteE (VarInit  name exp) env = do
  loc     <- derefEnv' name (actionEnv env)
  exp'    <- lift $ exp (actionEnv env)
  assign
    (loc, exp')

refName :: forall eff eff' v.
  (Heap v <: eff', Null <: v, Functor eff)
  => TEnv eff eff' (Fix v)
  -> VName -> Free eff' (TEnv eff eff' (Fix v))
refName env name = do
  (loc :: address ) <- ref (V.null :: Fix v)
  env'              <- refVar name loc $ actionEnv env
  return
    env { actionEnv = env' }

denoteP :: (Stream HtmlOut <: eff'
  , MLState Address v <: eff, Lift eff eff' v, Functor eff
  , MLState Address v <: eff', State Address <: eff'
  , Reader () (Maybe TId) <: eff', Random String String <: eff'
  , Writer TId <: eff', State TSeed <: eff'
  , v ~ Fix v', Null <: v') =>
    PageCall (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteP (PCall name args) env = do
  (PDef name params body) :: PageDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefPDef name env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  renderTag $ TagOpen "body" [("id", name)]
  isPageCall
  env'' <- storeTemplateId name env
  body env'' {actionEnv = env'}
  renderTag $ TagClose "body"


storeTemplateId name env = do
  tId :: Maybe TId <- readNext
  tId' <- case tId of 
    Just (TId tId) -> return tId
    Nothing  -> do
      seed :: TSeed <- get
      tId <- encode $ name ++ show seed 
      write $ TId tId
      return $ tId 
  return $ env {templateId  = tId' }

readInTemplateId :: (Reader () TId <: f) => TEnv g f v ->  Free f (TEnv g f v)
readInTemplateId env = do
  (TId tId) <- readNext
  return $ env {templateId  = tId }


derefPDef name = derefH name pagesH

