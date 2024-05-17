module Program.Syntax where
import Utils.Denote
import Utils.Fix
import Utils.Environment

data Program e f
    = Program [e]  -- real program will have an entry point 
    | Fragment [e] f -- no entrypoint: just a
    deriving Functor

type Name = String

