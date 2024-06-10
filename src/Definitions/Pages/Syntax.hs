module Definitions.Pages.Syntax where
import Syntax
import Definitions.Entity.Syntax (PName)

type PgName = String

data PageCall e = PCall PgName [(e, Type)]
  deriving Functor

data PageDef e 
    = PDef PgName [(PName, Type)] e
    deriving Functor

