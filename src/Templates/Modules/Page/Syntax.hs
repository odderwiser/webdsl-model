module Templates.Modules.Page.Syntax where
import Syntax (Type)
import Templates.Modules.Attributes.Syntax
import Definitions.Templates.Syntax


data Page e
    = PNavigate PgName [e] String
    | TCall TName (Attributes e) [(e, Type)] e e
    | Elements -- retrieve and evaluate elements
