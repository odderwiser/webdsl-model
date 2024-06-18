module Templates.Modules.Layout.Denotation where
import Utils
import Templates.Modules.Layout.Syntax
import Templates.Effects as E
import Templates.Modules.Attributes.Syntax (AttName, AttList)
import Text.HTML.TagSoup

denote :: forall eff eff' v. (E.Attribute <: eff', Stream HtmlOut <: eff',
    State AttList <: eff')
  => Layout (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denote (Header False e) env = do
    renderTag $ TagOpen "h1" [("class", "header section1")]
    e env
    renderTag $ TagClose "h1"

denote (Header True e) env = do
    (attributes :: AttList) <- get 
    renderTag $ TagOpen "h1" 
        $ ("class", "header section1") : attributes 
    e env
    renderTag $ TagClose "h1"

denote (Title e) env = do
    renderTitle e 

denote (Section False e) env = do
    classAttribute <- getAttribute
    renderTag $ TagOpen "span" 
        [("class", "section " ++ classAttribute)]
    renderSectionBody e env

denote (Section True e) env  = do
    (attributes :: AttList) <- get
    classAttribute          <- getAttribute
    renderTag $ TagOpen "span"
        $ ("class", "section " ++ classAttribute) : attributes 
    renderSectionBody e env

denote (Block False Nothing e) env = do
    renderTag $ TagOpen "div" [("class", "block")]
    renderBlockRmdr e env

denote (Block True Nothing e) env = do
    (attributes :: AttList) <- get
    renderTag $ TagOpen "div" $ ("class", "block") : attributes
    renderBlockRmdr e env 

denote (Block False (Just cName) e) env = do
    renderTag $ TagOpen "div" [("class", "block "++cName)]
    renderBlockRmdr e env

denote (Block True (Just cName) e) env = do
    (attributes :: AttList) <- get
    renderTag $ TagOpen "div" $ ("class", "block "++cName) : attributes
    renderBlockRmdr e env

denote (String string) env = do
    renderPlainText string False

renderSectionBody e env = do 
    increment
    e env
    decrement
    renderTag $ TagClose "span"

renderBlockRmdr e env  = do
    e env
    renderTag $ TagClose "block"
