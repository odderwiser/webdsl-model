module Templates.Effects where

import Utils
import Templates.Modules.Layout.Syntax hiding (Title)
import Templates.Modules.Attributes.Syntax
import Text.HTML.TagSoup (Tag (TagText, TagOpen), renderTags, escapeHTML)
import Actions.Effects (MLState, random, Random)

-- Layout ---
type IsEscaped = Bool

-- data RenderHtml k 
--     = RenderStartTag ClassName (Maybe (AttList String)) String k
--     | RenderOutput String IsEscaped k
--     | RenderString String k name="form_8548065150eda95eaa01b0c5e403383118308a5602" -- should this perhaps be escaped?
--     | RenderEndTag String k
--     | WriteTitle String k
--     | RenderLink String k
--     deriving Functor

data HtmlOut = HeaderOut | TitleOut | BodyOut | IsPageCall
-- data Content =  
data HtmlContent = Tag (Tag String) | Content 

data Stream t k 
    = Out t String k
    deriving Functor

isPageCall :: Stream HtmlOut <: f => Free f ()
isPageCall = Op $ inj $ Out IsPageCall "" $ Pure ()

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

reset :: (State ButtonCount <: f) => Free f ()
reset = Op $ inj $ PutS (Count 0)  $ Pure ()

data Render v k 
    = Render (Fix v) (String -> k)
    deriving Functor

--todo: rename to renderValue
render :: (Render v <: f) => Fix v -> Free f String
render v = Op $ inj $ Render v Pure

type ReqParamsSt = MLState String (Maybe String)
newtype Seed = Seed Int
    deriving (Eq, Num, Show)
type Label = String
type LabelId = String
newtype ButtonCount = Count Int
    deriving (Eq, Num)
type FormId = String

newtype TVarAddress = Address String
    deriving (Eq, Show)
newtype TVarSeed = VSeed Int
    deriving (Eq, Num, Show)
instance Show ButtonCount where
    show (Count i) = show i

encode ::  (Show e, Random Label v <: f) => e -> Free f v
encode = random

data Throw k = Throw String
    deriving Functor

throw :: (Throw <: f) => String -> Free f ()
throw errorMsg = Op $ inj $ Throw errorMsg