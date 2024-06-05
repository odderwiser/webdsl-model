module Templates.Modules.Layout.Denotation where
import Syntax
import Utils
import Templates.Modules.Layout.Syntax
import Templates.Effects as E
import Templates.Modules.Attributes.Syntax (AttName, AttList)
import Text.HTML.TagSoup

denote :: forall eff eff' v. (E.Attribute <: eff', Stream HtmlOut <: eff',
    State AttList <: eff')
  => Layout (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
denote (Header False e) eEnv pEnv h = do
    renderTag $ TagOpen "h1" [("class", "header section1")]
    e eEnv pEnv h
    renderTag $ TagClose "h1"

denote (Header True e) eEnv pEnv h = do
    (attributes :: AttList) <- get 
    renderTag $ TagOpen "h1" 
        $ ("class", "header section1") : attributes 
    e eEnv pEnv h
    renderTag $ TagClose "h1"

denote (Title e) eEnv pEnv h = do
    renderTitle e 

denote (Section False e) eEnv pEnv h = do
    classAttribute <- getAttribute
    renderTag $ TagOpen "span" 
        [("class", "section " ++ classAttribute)]
    renderSectionBody e eEnv pEnv h

denote (Section True e) eEnv pEnv h = do
    (attributes :: AttList) <- get
    classAttribute          <- getAttribute
    renderTag $ TagOpen "span"
        $ ("class", "section " ++ classAttribute) : attributes 
    renderSectionBody e eEnv pEnv h

denote (Block False Nothing e) eEnv pEnv h = do
    renderTag $ TagOpen "div" [("class", "block")]
    renderBlockRmdr e eEnv pEnv h

denote (Block True Nothing e)  eEnv pEnv h = do
    (attributes :: AttList) <- get
    renderTag $ TagOpen "div" $ ("class", "block") : attributes
    renderBlockRmdr e eEnv pEnv h 

denote (Block False (Just cName) e) eEnv pEnv h = do
    renderTag $ TagOpen "div" [("class", "block "++cName)]
    renderBlockRmdr e eEnv pEnv h

denote (Block True (Just cName) e) eEnv pEnv h = do
    (attributes :: AttList) <- get
    renderTag $ TagOpen "div" $ ("class", "block "++cName) : attributes
    renderBlockRmdr e eEnv pEnv h

denote (String string) eEnv pEnv h = do
    renderPlainText string False

renderSectionBody e eEnv pEnv h = do 
    increment
    e eEnv pEnv h
    decrement
    renderTag $ TagClose "span"

renderBlockRmdr e eEnv pEnv h = do
    e eEnv pEnv h 
    renderTag $ TagClose "block"
