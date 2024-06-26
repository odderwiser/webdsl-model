module Definitions.Program.Syntax where
import Utils.Denote
import Utils.Fix
import Utils.Environment
import Definitions.GlobalVars.Syntax (GlobalVar, DatabaseEntry, VarList)
import Network.HTTP.Types (Status, Method)
import Definitions.Pages.Syntax (RequestParams)

data Program e g f
    = Program [e] (Maybe g) -- real program will have an entry point 
    | Fragment [e] (Maybe g) f -- no entrypoint: just a
    | Request [e]  (Maybe g) (f, RequestParams)
    | Sequence [e] (Maybe g) [(f, RequestParams)]
    deriving Functor

-- data ProgramV g e f = WithVars [GlobalVar g] (Program e f) 
--     deriving Functor


-- type Path = [String]
-- data Request = Request Method Path RequestParams -- a test is definition and a set of requests
-- data Response = Response Status String -- unnecessary really 

type Name = String

