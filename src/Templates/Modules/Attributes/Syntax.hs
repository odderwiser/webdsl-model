module Templates.Modules.Attributes.Syntax where
import Utils.Fix
import Text.HTML.TagSoup (Attribute)
import Data.Bifunctor (Bifunctor (bimap))

type AttName = String
type AttList = [Attribute String]

data Attributes t a = SelectionList [AttributeSel t a] t
    deriving Functor

instance Bifunctor Attributes where
  bimap g f (SelectionList list body) = SelectionList (map (bimap g f) list) (g body)

data AttributeSel t a
    = Attribute AttName -- chose only one from scope 
    | AllAttributes [AttName] -- all attributes except on
    | AttDef AttName a
    deriving Functor

instance Bifunctor AttributeSel where
  bimap g f (Attribute name) = Attribute name
  bimap g f (AllAttributes l) = AllAttributes l
  bimap g f (AttDef name v) = AttDef name $ f v 