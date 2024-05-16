module Program.Syntax where

data Program e f
    = Program [e]  -- real program will have an entry point 
    | Fragment [e] f -- no entrypoint
    deriving Functor

type Name = String