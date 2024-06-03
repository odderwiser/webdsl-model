module Templates.Modules.Attributes.Syntax where
import Utils.Fix

type AttName = String
type AttList v = [(AttName, v)]

data Attributes e = SelectionList [e]
    deriving Functor

data AttributeSel e 
    = Attribute AttName -- chose only one from scope 
    | AllAttributes [AttName] -- all attributes except on
    | AttDef AttName e
    deriving Functor

