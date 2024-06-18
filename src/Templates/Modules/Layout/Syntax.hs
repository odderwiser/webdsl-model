{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Templates.Modules.Layout.Syntax where
import Utils.Denote
import Data.Bifunctor (Bifunctor (bimap))
import Utils.Composition (type (<::))
import Utils.Fix

type ClassName = String
type IsAttAssigned = Bool

data Layout t a
  = Header IsAttAssigned t
  | Title String
  -- | Description e  -- doesnt work anymore?
  | Section IsAttAssigned t 
  | String String
  | Block IsAttAssigned (Maybe ClassName) t
  deriving Functor


instance Bifunctor Layout where  
  bimap :: (a -> b) -> (c -> d) -> Layout a c -> Layout b d
  bimap g f (Header b e)  = Header b $ g e
  bimap g f (Title s)     = Title s
  bimap g f (Section b e) = Section b $ g e
  bimap g f (String s)    = String s
  bimap g f (Block a b c) = Block a b $ g c

header :: (Layout <:: f) => IsAttAssigned -> BiFix f (Fix e) -> BiFix f (Fix e)  
header iAtAsgd content = injBf $ Header iAtAsgd content

title :: (Layout <:: f) => String -> BiFix f (Fix e)
title = injBf . Title

section :: (Layout <:: f) => IsAttAssigned -> BiFix f (Fix e) -> BiFix f (Fix e)  
section iAtAsgd content = injBf $ Section iAtAsgd content

str :: (Layout <:: f) => String -> BiFix f (Fix e)
str = injBf . String

block :: (Layout <:: f) => IsAttAssigned -> Maybe ClassName -> BiFix f (Fix e) -> BiFix f (Fix e)  
block iAtAsgd cName content = injBf $ Block iAtAsgd cName content