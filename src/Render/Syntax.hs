module Render.Syntax where
import Data.Bifunctor (Bifunctor (bimap))

data Xml f e = Xml String (Maybe (f, Xml f e))
    
instance Bifunctor Xml where
  bimap :: (a -> b) -> (c -> d) -> Xml a c -> Xml b d
  bimap f g (Xml str Nothing)           = Xml str Nothing 
  bimap f g (Xml str (Just (exp, xml))) = Xml str 
    $ Just (f exp, bimap f g xml) 
