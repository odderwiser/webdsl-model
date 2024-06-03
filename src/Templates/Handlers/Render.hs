module Templates.Handlers.Render where
import Utils
import Templates.Effects


data PageR = PageR {
  title  :: Maybe String,
  body   :: String
}

renderHtmlH :: forall remEff val v. (Functor remEff)
  => Handler_ RenderHtml
  val PageR remEff (val, String)
renderHtmlH = Handler_ {
  ret_ = \x pageR -> pure (x, writeOut pageR),
  hdlr_ = \effect pageR         -> case effect of
    (RenderStartTag cName atts tag k) ->
      let  renderedTag = case atts of
            Nothing     -> "<" ++ tag ++ " "
              ++ renderAtt "class" cName ++ ">"
            (Just atts) -> "<" ++ tag ++ " "
              ++ renderAtt "class" cName
              ++ " " ++ mapAttributes atts ++ ">"
      in
      k $ writeBody renderedTag pageR
    (RenderPlainText string k)   -> k $  writeBody string pageR
    (RenderString string k)      -> k $  writeBody ("\""++string++"\"") pageR
    (RenderEndTag tag k)         ->
      let renderedTag = "</" ++ tag ++ ">" in
      k $ writeBody renderedTag pageR
    (WriteTitle title k)         -> k $ pageR { title = Just title }
  }

-- mapAttributes :: [(String, String)] -> String
mapAttributes defs = unwords $ map (\(k,v) -> renderAtt k (show v)) defs

renderAtt k v =  k ++ "=\"" ++ v ++ "\""

writeOut :: PageR -> [Char]
writeOut pageR =
  let title' = case title pageR of
        Nothing -> ""
        Just t  -> "<title>"
          ++ t
          ++ "</title>"
  in
    "<html><head>"
    ++ title'
    ++ "</head><body>"
    ++ body pageR
    ++ "</body></html>"


writeBody elem pageR = pageR { body = body pageR ++ elem}