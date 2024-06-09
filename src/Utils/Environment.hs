module Utils.Environment where
import Utils.Handler
import Utils.Free
import Actions.Modules.Eval.Syntax
import Syntax
import Data.Maybe (fromJust)
-- import Layout.Syntax (CName)
import Definitions.Templates.Syntax (PageDef, TemplateDef)
import Definitions.Fun.Syntax
import Definitions.Entity.Syntax
import Templates.Modules.Attributes.Syntax (AttName)
import Control.Natural

type Function eff v = FDecl (FreeEnv eff v)
type FreeEnv eff v = Env eff v -> Free eff v -- exp Env
type PEnv eff eff' v =  TEnv eff eff' v 
  -> Free eff' () --   THIS IS A REAL NT
type TClosure eff eff' v = (TEnv eff eff' v, PEnv eff eff' v)

-- data Entity eff e = Entity EName (Env eff e)  

data Env eff v = Env
    { varEnv     :: [(VName, Address)] -- this is for variables
    , defs       :: [Function eff v] -- this is for functions
    , entityDefs :: [EntityDef (FreeEnv eff v)] -- this is for all the entity definitions
    , objVarEnv  :: [(PName, Address)]
    -- , attributes :: [(AttName, String)]
    -- , pages      :: [(PgName, PageDef (FreeEnv eff v))]
    }

data TEnv eff eff' v =  TEnv
  { actionEnv   :: Env eff v
  , attributes  :: [(AttName, String)]
  , pages       :: [PEnv eff eff' v ]
  , templates   :: [TemplateDef (PEnv eff eff' v)]
  , elements    :: [(Address, TClosure eff eff' v)]
  }

-- discussion: couuld it be modelled differently, for example with higher order effects?
-- separation of effecta (ho effects)