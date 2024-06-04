module Templates.Modules.Render.Syntax where
import Data.Bifunctor (Bifunctor (bimap))

data Xml f e = Xml String (Maybe (f, Xml f e))

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


