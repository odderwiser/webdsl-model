module Templates.Modules.Page.Syntax where
import Syntax (Type (String))
import Templates.Modules.Attributes.Syntax
import Definitions.Templates.Syntax
import Data.Bifunctor (Bifunctor (bimap, first))
import Utils
import Definitions.Pages.Syntax (PgName)


data Page t a
  = PNavigate PgName [a] String
  | TCall TName [(a, Type)] (Maybe t) -- template name, attributes, args, elements???
  | Elements -- retrieve and evaluate elements
  deriving Functor

instance Bifunctor Page where
  bimap :: (a -> b) -> (c -> d) -> Page a c -> Page b d
  bimap g f(PNavigate name list path) = PNavigate name (map f list) path
  bimap g f (TCall name list elems) =
    TCall name (map (first f) list) $ fmap g elems
  bimap g f Elements = Elements


tCall :: (Page <:: f) => TName ->  [(Fix e, Type)] -> BiFix f e
tCall name args = injBf $ TCall name args Nothing 

tCallElems :: (Page <:: f) => TName ->  [(Fix e, Type)] -> BiFix f e -> BiFix f e
tCallElems name args elems = injBf $ TCall name args $ Just elems

elements :: (Page <:: f) => BiFix f e
elements = injBf Elements

newtype TId = TId String
  deriving (Show, Eq, Ord)
