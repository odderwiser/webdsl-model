module Templates.Modules.Render.Syntax where
import Data.Bifunctor (Bifunctor (bimap))
import Utils.Composition
import Utils

data Xml t a = Xml String (Maybe (a, Xml t a))
  deriving Functor

xmlT :: String -> Xml e f
xmlT f = Xml f Nothing 

xmlS :: String -> Fix e -> Xml f (Fix e) -> Xml f (Fix e)
xmlS start exp tail = Xml start $ Just (exp, tail)

instance Bifunctor Xml where
  bimap :: (a -> b) -> (c -> d) -> Xml a c -> Xml b d
  bimap g f (Xml str Nothing)           = Xml str Nothing 
  bimap g f (Xml str (Just (exp, xml))) = Xml str 
    $ Just (f exp, bimap g f xml) 

data Render t a 
  = XmlR (Xml t a)
  | Output a
  | Raw a
  deriving Functor
    
instance Bifunctor Render where
  bimap :: (a -> b) -> (c -> d) -> Render a c -> Render b d
  bimap g f (XmlR xml)       = XmlR $ bimap g f xml 
  bimap g f (Output actions) = Output (f actions) 
  bimap g f (Raw actions)    = Raw (f actions)

output :: (Render <:: f) => Fix h -> BiFix f (Fix h)
output = injBf . Output 

xml :: (Render <:: f) => String -> BiFix f (Fix h)
xml = injBf . XmlR . xmlT

xmlRec :: (Render <:: f) 
  => String -> Fix e 
  -> Xml (BiFix f (Fix e)) (Fix e)  -> BiFix f (Fix e)
xmlRec start exp tail = injBf $ XmlR $ xmlS start exp tail
