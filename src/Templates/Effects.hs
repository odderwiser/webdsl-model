module Templates.Effects where

import Utils
import Templates.Modules.Layout.Syntax hiding (Title)
import Templates.Modules.Attributes.Syntax
import Text.HTML.TagSoup (Tag (TagText, TagOpen), renderTags, escapeHTML)

-- Layout ---
type IsEscaped = Bool

-- data RenderHtml k 
--     = RenderStartTag ClassName (Maybe (AttList String)) String k
--     | RenderOutput String IsEscaped k
--     | RenderString String k -- should this perhaps be escaped?
--     | RenderEndTag String k
--     | WriteTitle String k
--     | RenderLink String k
--     deriving Functor

data HtmlOut = HeaderOut | TitleOut | BodyOut 
-- data Content =  
data HtmlContent = Tag (Tag String) | Content 

data Stream t k 
    = Out t String k
    deriving Functor

renderTag :: (Stream HtmlOut  <: f) 
    => Tag String -> Free f ()
renderTag tag = Op $ inj 
    $ Out BodyOut (renderTags [tag]) $ Pure ()

renderPlainText :: (Stream HtmlOut  <: f) 
    => String -> Bool -> Free f ()
renderPlainText string True = Op $ inj
    $ Out BodyOut (escapeHTML string)  $ Pure ()

renderPlainText string False = Op $ inj
    $ Out BodyOut string  $ Pure ()

renderAttributeValue string = Op $ inj
    $ Out BodyOut (show string) $ Pure ()

renderTitle :: (Stream HtmlOut <: f) => String -> Free f ()
renderTitle title = Op $ inj
    $ Out TitleOut title  $ Pure ()

renderLink text = Op $ inj
    $ Out BodyOut (renderTags [TagOpen "a" [("href", text)]]) $ Pure ()

data Attribute k
    = Increment k
    | Decrement k
    | Get (String -> k)
    deriving Functor

getAttribute :: (Attribute <: f) => Free f String
getAttribute = Op $ inj $ Get Pure

increment :: (Attribute <: f) => Free f ()
increment = Op $ inj $ Increment $ Pure ()

decrement :: (Attribute <: f) => Free f ()
decrement = Op $ inj $ Decrement $ Pure ()

data State v k
    = GetS (v -> k)
    | PutS v k
    deriving Functor

get :: (State v <: f) => Free f v 
get = Op $ inj $ GetS Pure

put v = Op $ inj $ PutS v $ Pure ()

data Render v k 
    = Render (Fix v) (String -> k)
    deriving Functor

--todo: rename to renderValue
render :: (Render v <: f) => Fix v -> Free f String
render v = Op $ inj $ Render v Pure