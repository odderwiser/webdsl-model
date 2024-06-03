module Definitions.Templates.Syntax where
import Definitions.Entity.Syntax (PName)
import Syntax (Type)

type PgName = String
type TName = String

data PageDef e 
    = PDef PgName [(PName, Type)] e
    deriving Functor

data TemplateDef e = TDef TName [(PName, Type)] e
    deriving Functor