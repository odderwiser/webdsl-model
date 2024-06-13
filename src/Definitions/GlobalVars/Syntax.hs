module Definitions.GlobalVars.Syntax where
import Actions.Modules.Eval.Syntax (Eval(Var), VName)
import Actions.Modules.Entity.Syntax (EntityDecl (EDecl), unbox, LitV)
import Utils.Fix
import Data.Maybe (fromJust)
import Utils.Composition

type DatabaseLoc = String --this contains file address to be checked
-- if file exists, do not load the variables, instead populate
    -- the env from file
-- if it doesn't, write to the file // populate the database

-- tests should be able to say, persistent / nonpersistent 

type Uuid = String

getUuid :: (LitV Uuid <: v) => EntityDecl (Fix v) -> Maybe Uuid
getUuid (EDecl name params) = unbox $ fromJust $ lookup "id" params

-- data LitId e = BoxId Uuid
--     deriving Functor

-- boxId (LitId e)

data GlobalVar e = VDef VName (EntityDecl e)
    deriving Functor

type DatabaseEntry = String

data VarList e = VList DatabaseEntry [GlobalVar e] e
getNames = map (\(VDef name e) -> name)