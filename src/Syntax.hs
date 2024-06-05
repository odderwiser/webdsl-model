module Syntax where
import Utils.Composition (type (<:))
import Utils.Free (Free)
import Utils.Fix
-- for now, they will be only included where they are necessary
type Address = Int

data Type  = Int | Bool | List | NullType | Entity
    deriving (Eq, Show)

data Null e = Null
    deriving (Functor, Eq, Show)

    

null :: (Null <: v) => Fix v 
null = injF Null
