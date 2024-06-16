module Templates.Modules.Forms.Denotation where
import qualified Templates.Effects as E
import Utils
import Templates.Effects
import Templates.Modules.Attributes.Syntax
import Templates.Modules.Forms.Syntax
import Text.HTML.TagSoup (Tag(TagOpen, TagClose))
import Actions.Values (unbox, Lit (V))
import Actions.Str
import Syntax 
import Actions.Bool (LitBool)

denoteR :: forall eff eff' v v'.
    ( E.Attribute <: eff', Stream HtmlOut <: eff'
    , State AttList <: eff', LitStr <: v', LitBool <: v'
    , v ~ Fix v'
    , Lift eff eff' v)
  => Forms (FreeEnv eff v) (PEnv eff eff' v)
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
        , ("class", "inputString")
        , ("value", "")] -- possibly inputtable

denoteR (Input writeLoc Int) env = do
    renderTag $ TagOpen "input" 
        [ ("class", "inputInt")
        , ("value", "0")] 
-- I should have cases for entities but I can't figure out how they work

denoteR (Submit action name) env = do
    name <- lift $ name $ actionEnv env
    renderTag $ TagOpen "button" 
        [ ("class", "button")] --name tba 
    renderPlainText (unbox name) True
    renderTag $ TagClose "button"