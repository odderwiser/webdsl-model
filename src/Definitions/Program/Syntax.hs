module Definitions.Program.Syntax where
import Utils.Denote
import Utils.Fix
import Utils.Environment
import Definitions.GlobalVars.Syntax (GlobalVar, DatabaseEntry)
import Network.HTTP.Types (Status, Method)

data Program e f
    = Program [e]  -- real program will have an entry point 
    | Fragment [e] f -- no entrypoint: just a
    deriving Functor

data ProgramV g e f = WithVars [GlobalVar g] (Program e f) 
    deriving Functor


-- type Path = [String]
-- data Request = Request Method Path RequestParams -- a test is definition and a set of requests
-- data Response = Response Status String -- unnecessary really 

type Name = String

