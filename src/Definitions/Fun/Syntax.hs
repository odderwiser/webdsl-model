module Definitions.Fun.Syntax where

type FunName = String
type ArgName = String

data FDecl e 
    = FDecl FunName [ArgName] e
    deriving (Functor, Eq)