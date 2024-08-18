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
import Actions.Effects (ref, MLState, Random, Writer, write)
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Modules.Eval.Denotation (refEnv')
import qualified Actions.Modules.Eval.Syntax
import Actions.Modules.Eval.Syntax
import Actions.Arith (LitInt)

denoteR :: forall eff eff' v v'.
  ( E.Attribute <: eff', Stream HtmlOut <: eff'
  , State AttList <: eff', State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff', State ButtonCount <: eff'
  , LitStr <: v', v ~ Fix v'
  , Lift eff eff' v)
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteR (Form False body) env = do -- here action should be determined? 
  seed :: Seed     <- get
  formId :: String <- encode $ "form_" ++ show seed
  put formId
  renderForm formId
  body env
  renderTag 
    $ TagClose "form"

denoteR (Label name contents) env = do -- attribute "for"
  name'                 <- lift $ name $ actionEnv env
  nameId :: LabelId     <- encode (unbox name' :: Label)
  put (Just nameId)
  put [("for", nameId)]
  attributes :: AttList <- get 
  renderTag $ TagOpen "label" attributes
  renderPlainText (unbox name') True
  renderTag $ TagClose "label"
  contents env

denoteR (Submit action name) env = do
  name                       <- lift $ name $ actionEnv env
  formId :: String           <- get 
  buttonCount :: ButtonCount <- get
  put [ ("class", "button"), ("name", "withForms_ia" ++ show buttonCount ++ "_" ++ formId)]
  attributes :: AttList      <- get
  renderTag $ TagOpen "button" attributes
  renderPlainText (unbox name) True
  renderTag $ TagClose "button"

denoteRInput :: forall eff eff' v v' g.
  ( E.Attribute <: eff', State AttList <: eff', Stream HtmlOut <: eff'
  , State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff', State ButtonCount <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lift eff eff' v, Denote g eff (Fix v'),
  Show (v' (Fix v')))
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteRInput (Input exp Bool) env = do
  exp'                  <- lift $ Utils.foldD exp $ actionEnv env
  label:: Maybe LabelId <- get
  seed :: Seed          <- get
  inputName              <- encode $ show label ++ show seed
  put ([("class", "inputBool"), ("type", "checkbox"), ("name", inputName)] 
    ++ evaluateBooleanValue (projF exp'))
  renderInput label

denoteRInput (Input exp String) env = do
  exp'         <- lift $ Utils.foldD exp $ actionEnv env
  value        <- case (projF exp':: Maybe (LitStr v)) of
    Just (V str) -> return [("value", str)]
    Nothing      -> return [] 
  label:: Maybe LabelId <- get
  seed :: Seed <- get
  inputName    <- encode $ show label ++ show seed
  put $ [("class", "inputString"), ("name", inputName), ("type", "text")] ++ value
  renderInput label


denoteRInput (Input exp Int) env = do
  exp'         <- lift $ Utils.foldD exp $ actionEnv env
  value        <- case (projF exp':: Maybe (LitInt v)) of
    Just (V int) -> return int
    Nothing      -> return 0 
  label:: Maybe LabelId <- get
  seed :: Seed <- get
  inputName    <- encode $ show label ++ show seed
  put [ ("class", "inputInt")
    , ("name", inputName)
    , ("value", show value)
    ]
  renderInput label

evaluateBooleanValue (Just (V True))  = [("value", "true")]
evaluateBooleanValue (Just (V False)) = [("value", "false")]
evaluateBooleanValue Nothing          = []

renderForm formId = renderTag $ TagOpen "form" 
    [ ("id", "form_"++formId), ("name", "form_"++formId)
    , ("accept-charset", "UTF-8"), ("method", "POST")]

renderInput :: (Stream HtmlOut <: f, State (Maybe LabelId) <: f, State AttList <: f) 
  => Maybe LabelId  -> Free f ()
renderInput (Just label) = do 
  put [("id", label)]
  atts :: AttList <- get
  renderTag $ TagOpen "input" atts

renderInput Nothing = do 
  atts :: AttList <- get
  renderTag $ TagOpen "input" atts

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
