module Templates.Modules.Page.Syntax where
import Syntax (Type)
import Templates.Modules.Attributes.Syntax
import Definitions.Templates.Syntax
import Data.Bifunctor (Bifunctor (bimap, first))
import Utils


data Page e f
    = PNavigate PgName [e] String
    | TCall TName (Attributes e) [(e, Type)] (Maybe f) -- template name, attributes, args, elements???
    | Elements -- retrieve and evaluate elements


instance Bifunctor Page where
  bimap :: (a -> b) -> (c -> d) -> Page a c -> Page b d
  bimap f g (PNavigate name list path) = PNavigate name (map f list) path
  bimap f g (TCall name atts list elems) =
    TCall name (fmap f atts) (map (first f) list) $ fmap g elems
  bimap f g Elements = Elements


tCall :: (Page <:: f) => TName ->  [(e, Type)] -> BiFix f e
tCall name args = injBf $ TCall name (SelectionList []) args Nothing 

tCallElems :: (Page <:: f) => TName ->  [(e, Type)] -> BiFix f e -> BiFix f e
tCallElems name args elems = injBf $ TCall name (SelectionList []) args $ Just elems