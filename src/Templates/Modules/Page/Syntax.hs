module Templates.Modules.Page.Syntax where
import Syntax (Type)
import Templates.Modules.Attributes.Syntax
import Definitions.Templates.Syntax
import Data.Bifunctor (Bifunctor (bimap, first))
import Utils
import Definitions.Pages.Syntax (PgName)


data Page t a
  = PNavigate PgName [a] String
  | TCall TName (Attributes a) [(a, Type)] (Maybe t) -- template name, attributes, args, elements???
  | Elements -- retrieve and evaluate elements
  deriving Functor

instance Bifunctor Page where
  bimap :: (a -> b) -> (c -> d) -> Page a c -> Page b d
  bimap g f(PNavigate name list path) = PNavigate name (map f list) path
  bimap g f (TCall name atts list elems) =
    TCall name (fmap f atts) (map (first f) list) $ fmap g elems
  bimap g f Elements = Elements


tCall :: (Page <:: f) => TName ->  [(e, Type)] -> BiFix f e
tCall name args = injBf $ TCall name (SelectionList []) args Nothing 

tCallElems :: (Page <:: f) => TName ->  [(e, Type)] -> BiFix f e -> BiFix f e
tCallElems name args elems = injBf $ TCall name (SelectionList []) args $ Just elems

elements :: (Page <:: f) => BiFix f e
elements = injBf Elements