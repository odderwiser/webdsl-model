module Templates.Modules.Forms.Syntax where
import Templates.Modules.Layout.Syntax (IsAttAssigned)
import Syntax (Type)
import Data.Bifunctor (Bifunctor (bimap))
import Utils.Composition
import Utils.Fix

data Param = Placeholder

data Forms e f 
    = Form IsAttAssigned f -- possibly consider making this a list instead??
    | Label e f -- first value evaluates to the text, the second to the field
    | Input e Type -- input with optional parameters. e is the bound writing but so far it only matters for evaluating the type of input!!
    | Submit e e -- fist thing is action, second is the string
    deriving Functor

instance Bifunctor Forms where
    bimap f g (Form bool body) = Form bool $ g body
    bimap f g (Label value contents) = Label (f value) (g contents)
    bimap f g (Input name ty) = Input (f name) ty
    bimap f g (Submit action name) = Submit (f action) (f name)

form :: (Forms <:: f) => IsAttAssigned -> BiFix f (Fix g) -> BiFix f (Fix g)
form isAssigned body = injBf $ Form isAssigned body

label :: (Forms <:: f) => Fix g -> BiFix f (Fix g) ->  BiFix f (Fix g)
label text body = injBf $ Label text body

input :: (Forms <:: f) => Fix g -> Type -> BiFix f (Fix g)
input var ty = injBf $ Input var ty

submit :: (Forms <:: f) => Fix g -> Fix g -> BiFix f (Fix g)
submit action name = injBf $ Submit action name