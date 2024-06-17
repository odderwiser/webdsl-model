module Definitions.Pages.Syntax where
import Syntax
import Definitions.Entity.Syntax (PName)
import Utils.Composition
import Data.Bifunctor (Bifunctor (bimap, first))

type PgName = String

data PageCall e f = PCall PgName [(e, Type)] RequestParams
  deriving Functor

pCallRoot = PCall "root" [] []

instance Bifunctor PageCall where
  bimap f g (PCall name types params) = PCall name (map (first f) types) params

type RequestParams = [(String, String)] -- parsable to headers

data PageDef e
    = PDef PgName [(PName, Type)] [e]
    deriving Functor

pDef :: (PageDef <: f) => PgName -> [(PName, Type)] -> [e] -> f e
pDef name args body = inj $ PDef name args body
