module Templates.Modules.Layout.Denotation where
import Syntax
import Utils
import Templates.Modules.Layout.Syntax
import Templates.Effects
import Templates.Modules.Attributes.Syntax (AttName, AttList)

denote :: forall eff eff' v. (Attribute <: eff', RenderHtml <: eff',
    State (AttList String) <: eff')
  => Layout (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
denote (Header False e) eEnv pEnv h = do
    renderStartTag "header section1" Nothing "h1" 
    e eEnv pEnv h
    renderEndTag "h1"

denote (Header True e) eEnv pEnv h = do
    (attributes :: AttList String) <- get 
    renderStartTag 
        "header section1" 
        (Just attributes) "h1" 
    e eEnv pEnv h
    renderEndTag "h1"

denote (Title e) eEnv pEnv h = do
    renderTitle e 

denote (Section False e) eEnv pEnv h = do
    classAttribute <- getAttribute
    renderStartTag 
        ("section " ++ classAttribute) 
        Nothing "span" 
    renderSectionBody e eEnv pEnv h

denote (Section True e) eEnv pEnv h = do
    (attributes :: AttList String) <- get
    classAttribute          <- getAttribute
    renderStartTag 
        ("section " ++ classAttribute) 
        (Just attributes) "span" 
    renderSectionBody e eEnv pEnv h

denote (Block False Nothing e) eEnv pEnv h = do
    renderStartTag "block" Nothing "div"
    renderBlockRmdr e eEnv pEnv h

denote (Block True Nothing e)  eEnv pEnv h = do
    (attributes :: AttList String) <- get
    renderStartTag "block" 
        (Just attributes) "div"
    renderBlockRmdr e eEnv pEnv h 

denote (Block False (Just cName) e) eEnv pEnv h = do
    renderStartTag 
        ("block " ++ cName)  
        Nothing "div"
    renderBlockRmdr e eEnv pEnv h

denote (Block True (Just cName) e) eEnv pEnv h = do
    (attributes :: AttList String) <- get
    renderStartTag 
        ("block " ++ cName)  
        (Just attributes) "div"
    renderBlockRmdr e eEnv pEnv h

denote (String string) eEnv pEnv h = do
    renderPlainText string False

renderSectionBody e eEnv pEnv h = do 
    increment
    e eEnv pEnv h
    decrement
    renderEndTag "span"

renderBlockRmdr e eEnv pEnv h = do
    e eEnv pEnv h 
    renderEndTag "block"
