{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Pages.Syntax where
import Syntax
import Definitions.Entity.Syntax (PName)
import Utils.Composition
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Templates.Modules.Forms.Syntax (EvalT)
import Definitions.Templates.Syntax (TBody (Body))

type PgName = String

data PageCall t a = PCall PgName [(a, Type)] RequestParams
  deriving Functor

pCallRoot = PCall "root" [] []

instance Bifunctor PageCall where
  bimap g f (PCall name types params) = PCall name (map (first f) types) params

type RequestParams = [(String, String)] -- parsable to headers

data PageDef t a
    = PDef PgName [(PName, Type)] t
    deriving Functor

instance Bifunctor PageDef where
  bimap g f (PDef name args elems) = PDef name args $ g elems

pDef :: (PageDef <:: f) => PgName -> [(PName, Type)] -> g -> f g h
pDef name args body = inj' $ PDef name args $ body

