module Layout.Denotation where
import Syntax
import Utils.Composition
import Layout.Syntax
import Utils.Environment
import Utils.Fix
import Layout.Effects
import Utils.Free
import Attributes.Syntax (AttName, AttList)

denote :: forall eff eff' v. (Attribute <: eff', RenderHtml <: eff',
    State (AttList String) <: eff')
  => Layout (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denote (Header False e) eEnv pEnv = do
    renderStartTag "header section1" Nothing "h1" 
    e eEnv pEnv
    renderEndTag "h1"

denote (Header True e) eEnv pEnv = do
    (attributes :: AttList String) <- get 
    renderStartTag 
        "header section1" 
        (Just attributes) "h1" 
    e eEnv pEnv
    renderEndTag "h1"

denote (Title e) eEnv pEnv = do
    renderTitle e 

denote (Section False e) eEnv pEnv = do
    classAttribute <- getAttribute
    renderStartTag 
        ("section " ++ classAttribute) 
        Nothing "span" 
    renderSectionBody e eEnv pEnv

denote (Section True e) eEnv pEnv = do
    (attributes :: AttList String) <- get
    classAttribute          <- getAttribute
    renderStartTag 
        ("section " ++ classAttribute) 
        (Just attributes) "span" 
    renderSectionBody e eEnv pEnv

denote (Block False Nothing e) eEnv pEnv = do
    renderStartTag "block" Nothing "div"
    renderBlockRmdr e eEnv pEnv

denote (Block True Nothing e)  eEnv pEnv = do
    (attributes :: AttList String) <- get
    renderStartTag "block" 
        (Just attributes) "div"
    renderBlockRmdr e eEnv pEnv

denote (Block False (Just cName) e) eEnv pEnv = do
    renderStartTag 
        ("block " ++ cName)  
        Nothing "div"
    renderBlockRmdr e eEnv pEnv

denote (Block True (Just cName) e) eEnv pEnv = do
    (attributes :: AttList String) <- get
    renderStartTag 
        ("block " ++ cName)  
        (Just attributes) "div"
    renderBlockRmdr e eEnv pEnv

denote (String string) eEnv pEnv = do
    renderString string

renderSectionBody e eEnv pEnv = do 
    increment
    e eEnv pEnv
    decrement
    renderEndTag "span"

renderBlockRmdr e eEnv pEnv = do
    e eEnv pEnv
    renderEndTag "block"
