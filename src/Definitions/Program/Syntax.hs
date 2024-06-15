module Definitions.Program.Syntax where
import Utils.Denote
import Utils.Fix
import Utils.Environment
import Definitions.GlobalVars.Syntax (GlobalVar, DatabaseEntry)

data Program e f
    = Program [e]  -- real program will have an entry point 
    | Fragment [e] f -- no entrypoint: just a
    deriving Functor

data ProgramV g e f = WithVars [GlobalVar g] (Program e f)
    deriving Functor

type Name = String

