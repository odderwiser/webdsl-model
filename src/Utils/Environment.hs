module Utils.Environment where
import Utils.Handler
import Utils.Free
import Actions.Modules.Eval.Syntax
import Syntax
import Data.Maybe (fromJust)
-- import Layout.Syntax (CName)
import Attributes.Syntax (AttName)
import Page.Syntax (PageDef, PgName)
import Definitions.Fun.Syntax
import Definitions.Entity.Syntax

type Function eff v = FDecl (FreeEnv eff v)
type FreeEnv eff v = Env eff v -> Free eff v -- exp Env
type PEnv eff eff' v = Env eff v -> TEnv eff eff' v 
  -> (Free eff v -> Free eff' v) -> Free eff' ()


-- data Entity eff e = Entity EName (Env eff e)  

data Env eff v = Env
    { varEnv     :: [(VName, Address)] -- this is for variables
    , defs       :: [Function eff v] -- this is for functions
    , entityDefs :: [EntityDef (FreeEnv eff v)] -- this is for all the entity definitions
    , objVarEnv  :: [(PName, Address)]
    -- , attributes :: [(AttName, String)]
    -- , pages      :: [(PgName, PageDef (FreeEnv eff v))]
    }

data TEnv eff eff' v = TEnv
  {  attributes :: [(AttName, String)]
  , pages      :: [PEnv eff eff' v]
  }
