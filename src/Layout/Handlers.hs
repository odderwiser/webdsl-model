module Layout.Handlers where
import Utils.Handler
import Layout.Effects
import Attributes.Syntax
import Data.Maybe (fromJust)
import Data.List (intersperse, delete)
import Utils.Fix

data PageR = PageR {
  title :: String,
  body   :: String
}

renderHtmlH :: forall remEff val v. (Functor remEff, Show (Fix v))
  => Handler_ RenderHtml
  val PageR remEff String
renderHtmlH = Handler_ {
  ret_ = \x pageR -> pure $ writeOut pageR,
  hdlr_ = \effect pageR -> case effect of
    (RenderStartTag cName atts tag k) ->
      let  renderedTag = case atts of 
            Nothing     -> "<" ++ tag ++ " "
              ++ renderAtt "class" cName ++ ">" 
            (Just atts) -> "<" ++ tag ++ " "
              ++ renderAtt "class" cName 
              ++ " " ++ mapAttributes atts ++ ">"
      in
      k $ writeBody renderedTag pageR
    (RenderString string k)      -> k $ writeBody string pageR
    (RenderEndTag tag k)         ->
      let renderedTag = "</" ++ tag ++ ">" in
      k $ writeBody renderedTag pageR
    (WriteTitle title k)         -> k $ pageR { title = title }
  }

-- mapAttributes :: [(String, String)] -> String
mapAttributes defs = unwords $ map (\(k,v) -> renderAtt k (show v)) defs

renderAtt k v =  k ++ "\"" ++ v ++ "\"."

writeOut pageR = "<html><head><title>"
  ++ title pageR
  ++ "</title></head><body>"
  ++ body pageR
  ++ "</body></html>"

writeBody elem pageR = pageR { body = body pageR ++ elem}

attributeH :: (Functor remEff) =>
  Handler_ Attribute val (String, Int)
  remEff ()
attributeH = Handler_ {
  ret_ = \x rep -> pure ()
  ,hdlr_ = \eff (name, v) -> case eff of
    (Increment k) -> k (name, v+1)
    (Decrement k) -> k (name, v-1)
    (Get k)       -> k (name++ show v) (name, v)
}
