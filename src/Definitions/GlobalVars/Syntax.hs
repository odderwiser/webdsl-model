module Definitions.GlobalVars.Syntax where
import Actions.Modules.Eval.Syntax (Eval(Var), VName)
import Actions.Modules.Entity.Syntax (EntityDecl (EDecl))
import Utils.Fix
import Data.Maybe (fromJust)
import Utils.Composition
import Actions.Values
import Templates.Modules.Lift.Syntax
import Data.Bifunctor

type DatabaseLoc = String --this contains file address to be checked
-- if file exists, do not load the variables, instead populate
    -- the env from file
-- if it doesn't, write to the file // populate the database

-- tests should be able to say, persistent / nonpersistent 

type Uuid = String

getUuid :: (Lit Uuid <: v) => EntityDecl (Fix v) -> Uuid
getUuid (EDecl name params) = unbox $ fromJust $ lookup "id" params

-- data LitId e = BoxId Uuid
--     deriving Functor

-- boxId (LitId e)

data GlobalVar e = VDef VName (EntityDecl e)
    deriving Functor

type DatabaseEntry = String

data VarListT e f = VList [GlobalVar e] f
    deriving Functor

instance Bifunctor VarListT where 
    bimap f g (VList list cont) = VList (map (fmap f) list) $ g cont

type VarList = Weaken VarListT
getNames = map (\(VDef name e) -> name)