module Templates.Effects where

import Utils
import Templates.Modules.Layout.Syntax
import Templates.Modules.Attributes.Syntax

-- Layout ---
type IsEscaped = Bool

data RenderHtml k 
    = RenderStartTag ClassName (Maybe (AttList String)) String k
    | RenderOutput String IsEscaped k
    | RenderString String k -- should this perhaps be escaped?
    | RenderEndTag String k
    | WriteTitle String k
    | RenderLink String k
    deriving Functor

renderStartTag :: (RenderHtml <: f) 
    => ClassName -> Maybe (AttList String) -> String -> Free f ()
renderStartTag name list tag = Op $ inj 
    $ RenderStartTag name list tag $ Pure ()

renderEndTag tag = Op $ inj 
    $ RenderEndTag tag $ Pure ()

renderPlainText string isEscaped = Op $ inj
    $ RenderOutput string isEscaped $ Pure ()

renderString string = Op $ inj
    $ RenderString string $ Pure ()

renderTitle :: (RenderHtml <: f) => String -> Free f ()
renderTitle title = Op $ inj
    $ WriteTitle title $ Pure ()

renderLink text = Op $ inj
    $ RenderLink text $ Pure ()

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