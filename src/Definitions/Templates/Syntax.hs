{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Templates.Syntax where
import Definitions.Entity.Syntax (PName)
import Syntax (Type)
import Utils.Composition
import Utils.Fix (BiFix, Fix, injBf)
import Templates.Modules.Forms.Syntax (EvalT)
import Data.Bifunctor

type TName = String

data TemplateDef t a = TDef TName [(PName, Type)] t
  deriving Functor

data StatementType = Definition | Expression
  deriving Eq

data TBody t a = Body [String] t t -- body contains list of names in scope, list of definitions and list of remaining statements
  deriving Functor

body :: (TBody <:: f) => [String] -> BiFix f (Fix g) -> BiFix f (Fix g) -> BiFix f (Fix g)
body names defs stmts = injBf $ Body names defs stmts

instance Bifunctor TBody where
  bimap g f (Body names defs elems) = Body names (g defs) (g elems)

instance Bifunctor TemplateDef where
  bimap g f (TDef name args elems) = TDef name args
    $ g elems

tDef :: (TemplateDef <:: f) => TName -> [(PName, Type)] -> t -> f t a
tDef name args body = inj' $ TDef name args body