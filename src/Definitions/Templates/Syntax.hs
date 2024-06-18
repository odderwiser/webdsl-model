{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Templates.Syntax where
import Definitions.Entity.Syntax (PName)
import Syntax (Type)
import Utils.Composition
import Utils.Fix (BiFix)
import Templates.Modules.Forms.Syntax (EvalT)
import Data.Bifunctor

type TName = String

data TemplateDef t a = TDef TName [(PName, Type)] (TBody t a)
  deriving Functor

data TBody t a = Body [EvalT t a \/ t]

instance Functor (TBody t) where
  fmap f (Body elems) = Body $ map (\e -> case e of
    (Left evalT) -> Left $ fmap f evalT
    (Right x) -> Right $ x) elems

instance Bifunctor TBody where
  bimap g f (Body elems) = Body
    $ map (\e -> case e of
      (Left eval) -> Left $ bimap g f eval
      (Right x) -> Right $ g x) elems  

instance Bifunctor TemplateDef where
  bimap g f (TDef name args elems) = TDef name args
    $ bimap g f elems

tDef :: (TemplateDef <:: f) => TName -> [(PName, Type)] -> [EvalT t a \/ t] -> f t a
tDef name args body = inj' $ TDef name args $ Body body