module Actions.Modules.Phases.Syntax where


data VTuple e = Validate e String
    deriving (Functor, Eq, Show)