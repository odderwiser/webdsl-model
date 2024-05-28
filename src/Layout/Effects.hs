module Layout.Effects where
import Layout.Syntax
import Utils.Free
import Utils.Composition
import Attributes.Syntax (Attributes, AttName, AttList)
import Utils.Fix

data RenderHtml k 
    = RenderStartTag CName (Maybe (AttList String)) String k
    | RenderPlainText String k
    | RenderString String k
    | RenderEndTag String k
    | WriteTitle String k
    | RenderLink String k
    deriving Functor

renderStartTag :: (RenderHtml <: f) 
    => CName -> Maybe (AttList String) -> String -> Free f ()
renderStartTag name list tag = Op $ inj 
    $ RenderStartTag name list tag $ Pure ()

renderEndTag tag = Op $ inj 
    $ RenderEndTag tag $ Pure ()

renderPlainText string = Op $ inj
    $ RenderPlainText string $ Pure ()

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