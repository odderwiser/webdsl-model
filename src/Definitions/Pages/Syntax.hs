module Definitions.Pages.Syntax where
import Syntax
import Definitions.Entity.Syntax (PName)
import Utils.Composition

type PgName = String

data PageCall e = PCall PgName [(e, Type)]
  deriving Functor

data PageDef e 
    = PDef PgName [(PName, Type)] e
    deriving Functor

pDef :: (PageDef <: f) => PgName -> [(PName, Type)] -> e -> f e 
pDef name args body = inj $ PDef name args body
