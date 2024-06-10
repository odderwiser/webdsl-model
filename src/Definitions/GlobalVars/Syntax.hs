module Definitions.GlobalVars.Syntax where
import Actions.Modules.Eval.Syntax (Eval(Var), VName)
import Actions.Modules.Entity.Syntax (EntityDecl)

type Database = String --this contains file address to be checked
-- if file exists, do not load the variables, instead populate
    -- the env from file
-- if it doesn't, write to the file // populate the database

-- tests should be able to say, persistent / nonpersistent 


data GlobalVar e = VDef VName (EntityDecl e)
    deriving Functor