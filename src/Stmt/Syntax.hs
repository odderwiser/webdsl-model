module Stmt.Syntax where
import GHC.OldList (elemIndex)

data Stmt e =  S e e
    deriving Functor