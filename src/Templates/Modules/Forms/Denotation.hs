module Templates.Modules.Forms.Denotation where
import qualified Templates.Effects as E
import Utils
import Templates.Effects
import Templates.Modules.Attributes.Syntax
import Templates.Modules.Forms.Syntax
import Text.HTML.TagSoup (Tag(TagOpen, TagClose))
import Actions.Values (unbox, Lit (V), Null (Null))
import Actions.Str
import Syntax
import Actions.Bool (LitBool)
import Actions.Effects (ref, MLState)
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Modules.Eval.Denotation (refEnv')
import qualified Actions.Modules.Eval.Syntax
import Actions.Modules.Eval.Syntax

denoteR :: forall eff eff' v v'.
    ( E.Attribute <: eff', Stream HtmlOut <: eff'
    , State AttList <: eff', LitStr <: v', LitBool <: v'
    , v ~ Fix v'
    , Lift eff eff' v)
  => Forms (PEnv eff eff' v) (FreeEnv eff v) 
  -> PEnv eff eff' v
denoteR (Form False body) env = do -- here name and id is generated, and action should be determined? 
    renderTag $ TagOpen "form" [("accept-charset", "UTF-8"), ("method", "POST")]
    body env
    renderTag $ TagClose "form"

denoteR (Label name contents) env = do -- attribute "for"
    renderTag $ TagOpen "label" []
    name' <- lift $ name $ actionEnv env
    renderPlainText (unbox name') True
    renderTag $ TagClose "label"
    contents env

denoteR (Input writeLoc Bool) env = do
    renderTag $ TagOpen "input"
        [ ("type", "checkbox")
        , ("class", "inputBool")]

denoteR (Input writeLoc String) env = do
    renderTag $ TagOpen "input"
        [ ("type", "text")
       -- , ("value", "")
        , ("class", "inputString")
        ] -- possibly inputtable

denoteR (Input writeLoc Int) env = do
    renderTag $ TagOpen "input"
        [ ("value", "0")
        , ("class", "inputInt")
        ]
-- I should have cases for entities but I can't figure out how they work

denoteR (Submit action name) env = do
    name <- lift $ name $ actionEnv env
    renderTag $ TagOpen "button"
        [ ("class", "button")] --name tba 
    renderPlainText (unbox name) True
    renderTag $ TagClose "button"

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