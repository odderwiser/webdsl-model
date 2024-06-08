module Templates.Modules.Render.Syntax where
import Data.Bifunctor (Bifunctor (bimap))
import Utils.Composition
import Utils

data Xml e f = Xml String (Maybe (e, Xml e f))

xmlT :: String -> Xml e f
xmlT f = Xml f Nothing 

xmlS :: String -> Fix e -> Xml (Fix e) f -> Xml (Fix e) f
xmlS start exp tail = Xml start $ Just (exp, tail)

instance Bifunctor Xml where
  bimap :: (a -> b) -> (c -> d) -> Xml a c -> Xml b d
  bimap f g (Xml str Nothing)           = Xml str Nothing 
  bimap f g (Xml str (Just (exp, xml))) = Xml str 
    $ Just (f exp, bimap f g xml) 

data Render f e 
  = XmlR (Xml f e)
  | Output f
  | Raw f
    
instance Bifunctor Render where
  bimap :: (a -> b) -> (c -> d) -> Render a c -> Render b d
  bimap f g (XmlR xml)       = XmlR $ bimap f g xml 
  bimap f g (Output actions) = Output (f actions) 
  bimap f g (Raw actions)    = Raw (f actions)

output :: (Render <:: f) => Fix h -> BiFix f (Fix h)
output = injBf . Output 

xml :: (Render <:: f) => String -> BiFix f (Fix h)
xml = injBf . XmlR . xmlT

xmlRec :: (Render <:: f) 
  => String -> Fix e 
  -> Xml (Fix e) (BiFix f (Fix e)) -> BiFix f (Fix e)
xmlRec start exp tail = injBf $ XmlR $ xmlS start exp tail
