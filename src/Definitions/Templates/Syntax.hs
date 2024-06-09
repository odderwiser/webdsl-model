module Definitions.Templates.Syntax where
import Definitions.Entity.Syntax (PName)
import Syntax (Type)
import Utils.Composition
import Utils.Fix (BiFix)

type PgName = String
type TName = String

data PageDef e 
    = PDef PgName [(PName, Type)] e
    deriving Functor

data TemplateDef e = TDef TName [(PName, Type)] e
    deriving Functor

tDef :: (TemplateDef <: f) => TName -> [(PName, Type)] -> e -> f e 
tDef name args body = inj $ TDef name args body