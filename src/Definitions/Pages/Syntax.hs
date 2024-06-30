{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Pages.Syntax where
import Syntax
import Definitions.Entity.Syntax (PName)
import Utils.Composition
import Data.Bifunctor (Bifunctor (bimap, first, second))
import Templates.Modules.Forms.Syntax (EvalT)
import Definitions.Templates.Syntax (TBody (Body))
import Utils.Fix (BiFix, Fix, injBf)

type PgName = String

data PageCall t a = PCall PgName [(a, Type)]
  deriving Functor

pCallRoot = PCall "root" []

pCall :: (PageCall <:: f) => String -> BiFix f (Fix g)
pCall name = injBf $ PCall name []

instance Bifunctor PageCall where
  bimap g f (PCall name types) = PCall name (map (first f) types) 

type RequestParams = [(String, String)] -- parsable to headers

data PageDef t a
    = PDef PgName [(PName, Type)] t
    deriving Functor

instance Bifunctor PageDef where
  bimap g f (PDef name args elems) = PDef name args $ g elems

pDef :: (PageDef <:: f) => PgName -> [(PName, Type)] -> g -> f g h
pDef name args body = inj' $ PDef name args $ body

