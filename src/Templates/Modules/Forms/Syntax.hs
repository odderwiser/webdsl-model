module Templates.Modules.Forms.Syntax where
import Syntax (Type)
import Data.Bifunctor (Bifunctor (bimap))
import Utils.Composition
import Utils.Fix
import Actions.Modules.Eval.Syntax (VName)
import Definitions.Entity.Syntax (PName)
import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Actions.Values (Uuid)

data Param = Placeholder
type IsAttAssigned' = Bool


data Forms t a 
    = Form IsAttAssigned' t -- possibly consider making this a list instead??
    | Label a t -- first value evaluates to the text, the second to the field
    | Submit t a -- fist thing is action, second is the string
    deriving Functor

data Input r t a = Input r Type -- input with optional parameters. e is the bound writing but so far it only matters for evaluating the type of input!!
    deriving Functor

instance Bifunctor Forms where
    bimap :: (a -> b) -> (c -> d) -> Forms a c -> Forms b d
    bimap g f (Form bool body) = Form bool $ g body
    bimap g f (Label value contents) = Label (f value) (g contents)
    bimap g f (Submit action name) = Submit (g action) (f name)

instance Bifunctor (Input r) where
    bimap g f (Input r ty) = Input r ty

form :: (Forms <:: f) => IsAttAssigned' -> BiFix f g -> BiFix f g
form isAssigned body = injBf $ Form isAssigned body

label :: (Forms <:: f) => Fix g -> BiFix f g ->  BiFix f g
label text body = injBf $ Label text body

input :: (Input (Fix g) <:: f) => Fix g -> Type -> BiFix f g
input var ty = injBf $ Input var ty

submit :: (Forms <:: f) => BiFix f g -> Fix g -> BiFix f g
submit action name = injBf $ Submit action name

data EvalT t a = VarDeclT VName | VarInit VName a 
    deriving Functor

instance Bifunctor EvalT where
    bimap g f (VarDeclT name) = VarDeclT name
    bimap g f (VarInit name v) = VarInit name (f v)

vDeclT :: (EvalT <:: f) => VName -> BiFix f g
vDeclT = injBf . VarDeclT

varInitT :: (EvalT <:: f) => VName -> Fix g -> BiFix f g
varInitT name v = injBf $ VarInit name v 

data PropRef e = PropRef (Uuid, PName)
    deriving (Functor, Eq, Show, Generic)

instance ToJSON (PropRef e)
instance FromJSON (PropRef e)