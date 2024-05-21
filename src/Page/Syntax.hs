module Page.Syntax where
import Syntax (Type)
import Entity.Syntax (PName)
import Attributes.Syntax

type PgName = String
type TName = String

data PageDef e 
    = PDef PgName [(PName, Type)] e
    deriving Functor

data TemplateDef e = TDef TName [(PName, Type)] e
    deriving Functor

data Page e
    = PNavigate PgName [e] String
    | TCall TName (Attributes e) [(e, Type)] e e
    | Elements -- retrieve and evaluate elements
