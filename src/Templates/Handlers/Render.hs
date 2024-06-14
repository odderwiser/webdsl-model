module Templates.Handlers.Render where
import Utils
import Templates.Effects
import Text.HTML.TagSoup (escapeHTML)
import Actions.Str as S
import Actions.Arith as A
import Actions.Bool as B
import Actions.Syntax (projC)
import Actions.Values


data PageR = PageR {
  title  :: Maybe String,
  body   :: String
}

renderHtmlH :: forall remEff val v. (Functor remEff)
  => Handler_ (Stream HtmlOut)
  val PageR remEff (val, String)
renderHtmlH = Handler_ {
  ret_ = \x pageR -> pure (x, writeOut pageR),
  hdlr_ = \effect pageR -> case effect of
    (Out BodyOut str k)    ->
      k $ writeBody str pageR
    (Out TitleOut title k) -> k $ pageR { title = Just title }
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

renderH :: (Functor remEff, [] <: v, LitInt <: v, LitStr <: v, LitBool <: v)
  => Handler (Render v)
  val remEff val
renderH = Handler {
  ret = pure,
  hdlr = \(Render v k) -> k $ show' $ coerceTypes v 
}

coerceTypes :: ([] <: v', LitInt <: v', LitStr <: v', LitBool <: v')
  => Fix v' -> Fix ([] + LitInt + LitStr + LitBool)
coerceTypes e = case projF e of
  Just (Box (int :: Int)) -> box int
  Nothing -> case projF e of
    Just (Box (bool :: Bool)) -> box bool
    Nothing -> case projF e of
      Just (Box (str :: String)) -> box str
      Nothing -> case projC e of
        list -> injF $ map coerceTypes list

class (Show' e) where
  show':: e -> String

instance (Show' (f (Fix f))) => Show' (Fix f) where
  show' :: Show' (f (Fix f)) => Fix f -> String
  show' (In x) = show' x

instance (Show' (a e), Show' (b e)) => Show' ((a + b) e) where
  show' :: (Show' (a e), Show' (b e)) => (+) a b e -> String
  show' (L a) = show' a
  show' (R b) = show' b

instance Show' (LitStr e) where
  show' :: LitStr e -> String
  show' (Box str) = str

instance Show' (LitInt e) where
  show' :: LitInt e -> String
  show' (Box int) = show int

instance Show' (LitBool e) where
  show' :: LitBool e -> String
  show' (Box bool) = "<input type=\"checkbox\" checked="
    ++ show bool
    ++ " disabled=\"true\">"

instance (Show' e) => Show' [e] where
  show' :: [e] -> String
  show' list = "<ul class=\"block\">"
    ++ concatMap (\elem -> "::-marker" ++ show' elem) list
    ++ "</ul>"
