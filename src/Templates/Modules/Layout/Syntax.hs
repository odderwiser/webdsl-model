{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use newtype instead of data" #-}
module Templates.Modules.Layout.Syntax where
import Utils.Denote
import Data.Bifunctor (Bifunctor (bimap))

type CName = String

data Layout f e
    = Header Bool e
    | Title String
    -- | Description e  -- doesnt work anymore?
    | Section Bool e 
    | String String
    | Block Bool (Maybe CName) e

data Output e = Output e

instance Bifunctor Layout where  
  bimap :: (a -> b) -> (c -> d) -> Layout a c -> Layout b d
  bimap f g (Header b e)  = Header b $ g e
  bimap f g (Title s)     = Title s
  bimap f g (Section b e) = Section b $ g e
  bimap f g (String s)    = String s
  bimap f g (Block a b c) = Block a b $ g c

