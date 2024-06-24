module Templates.Modules.Forms.Denotation where
import qualified Templates.Effects as E
import Utils
import Templates.Effects
import Templates.Modules.Attributes.Syntax
import Templates.Modules.Forms.Syntax
import Text.HTML.TagSoup as H (Tag(TagOpen, TagClose), Attribute)
import Actions.Values (unbox, Lit (V), Null (Null))
import Actions.Str
import Syntax
import Actions.Bool (LitBool)
import Actions.Effects (ref, MLState, Random)
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Modules.Eval.Denotation (refEnv')
import qualified Actions.Modules.Eval.Syntax
import Actions.Modules.Eval.Syntax
import Actions.Arith (LitInt)

denoteR :: forall eff eff' v v'.
  ( E.Attribute <: eff', Stream HtmlOut <: eff'
  , State AttList <: eff', State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff', State ButtonCount <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lift eff eff' v)
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteR (Form False body) env = do -- here action should be determined? 
  seed :: Seed <- get
  formId :: String <- encode $ "form_" ++ show seed
  put formId
  renderTag $ TagOpen "form" 
    [ ("id", "form_"++formId), ("name", "form_"++formId)
    , ("accept-charset", "UTF-8"), ("method", "POST")]
  body env
  renderTag $ TagClose "form"
  reset -- this breaks if there are nested forms. Why would there be nested forms? 

denoteR (Label name contents) env = do -- attribute "for"
  name'      <- lift $ name $ actionEnv env
  nameId     <- encode (unbox name' :: Label)
  put       (Just nameId)
  renderTag $ TagOpen "label" [("for", nameId)]
  renderPlainText 
    (unbox name') True
  renderTag $ TagClose "label"
  contents env

denoteR (Input exp Bool) env = do
  exp'         <- lift $ exp $ actionEnv env
  value        <- case (projF exp':: Maybe (LitBool v)) of
    Just (V True)  -> return [("value", "true")]
    Just (V False) -> return [("value", "false")]
    Nothing        -> return []
  label        <- get
  seed :: Seed <- get
  inputName    <- encode $ show label ++ show value ++  show seed
  renderInput label
    $  [("class", "inputBool"), ("type", "checkbox"), ("name", inputName)] 
    ++ value

denoteR (Input exp String) env = do
  exp'         <- lift $ exp $ actionEnv env
  value        <- case (projF exp':: Maybe (LitStr v)) of
    Just (V str) -> return [("value", str)]
    Nothing      -> return [] 
  label        <- get
  seed :: Seed <- get
  inputName    <- encode $ show label ++ show value ++ show seed
  renderInput label
    $  [("class", "inputString"), ("name", inputName), ("type", "text")] 
    ++ value


denoteR (Input exp Int) env = do
  exp'         <- lift $ exp $ actionEnv env
  value        <- case (projF exp':: Maybe (LitInt v)) of
    Just (V int) -> return int
    Nothing      -> return 0 
  label        <- get
  seed :: Seed <- get
  inputName    <- encode $ show label ++ show value ++ show seed
  renderInput label
    [ ("class", "inputInt")
    , ("name", inputName)
    , ("value", show value)
    ]
-- I should have cases for entities but I can't figure out how they work

denoteR (Submit action name) env = do
  name <- lift $ name $ actionEnv env
  formId :: String <- get 
  buttonCount :: ButtonCount <- get
  renderTag $ TagOpen "button"
    [ ("class", "button")
    , ("name", "withForms_ia" ++ show buttonCount ++ "_" ++ formId)]
  renderPlainText (unbox name) True
  renderTag $ TagClose "button"

renderInput :: (Stream HtmlOut <: f, State (Maybe LabelId) <: f) 
  => Maybe LabelId ->  [H.Attribute String] -> Free f ()
renderInput label atts = do 
  id <- case label of 
    Just label -> return [("id", label)]
    Nothing    -> return []  
  renderTag $ TagOpen "input" $ id ++ atts

denoteNames ::  forall eff eff' v.
  ( Heap v <: eff', Null <: v
  ) => EvalT (FreeEnv eff (Fix v)) (PEnv eff eff' (Fix v))
  -> Env eff (Fix v)
  -> Free eff' (Env eff (Fix v))
denoteNames (VarDeclT name) env = refName name env

denoteNames (VarInit name e) env = refName name env

refName :: forall eff eff' v. (Heap v <: eff', Null <: v)
  => VName -> Env eff (Fix v) -> Free eff' (Env eff (Fix v))
refName name env = do
  (loc :: Address) <- ref (injF Null :: Fix v)
  refEnv' name loc env
