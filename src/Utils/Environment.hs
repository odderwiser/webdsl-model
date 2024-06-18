module Utils.Environment where
import Utils.Handler
import Utils.Free
import Actions.Modules.Eval.Syntax
import Syntax
import Data.Maybe (fromJust)
-- import Layout.Syntax (CName)
import Definitions.Templates.Syntax (TemplateDef)
import Definitions.Fun.Syntax
import Definitions.Entity.Syntax
import Templates.Modules.Attributes.Syntax (AttName)
import Definitions.Pages.Syntax (PageDef)
import Actions.Modules.Entity.Syntax (EntityDecl)
import Utils.Fix
import Templates.Modules.Forms.Syntax (EvalT)
import Utils.Composition

type Function eff v = FDecl (FreeEnv eff v)
type FreeEnv eff v = Env eff v -> Free eff v -- exp Env
type PEnv eff eff' v =  TEnv eff eff' v 
  -> Free eff' () --   THIS IS A REAL NT
type TClosure eff eff' v = (TEnv eff eff' v, PEnv eff eff' v)

-- data Entity eff e = Entity EName (Env eff e)  

data Env eff v = Env
    { varEnv     :: [(VName, Address)] -- this is for variables
    , globalVars :: [(VName, Address)] 
    , defs       :: [Function eff v] -- this is for functions
    , entityDefs :: [EntityDef (FreeEnv eff v)] -- this is for all the entity definitions
    , objVarEnv  :: [(PName, Address)]
    }

data TEnv eff eff' v =  TEnv
  { actionEnv   :: Env eff v
  , attributes  :: [(AttName, String)]
  , pages       :: [PageDef (PEnv eff eff' v) (FreeEnv eff v)]
  , templates   :: [TemplateDef (PEnv eff eff' v) (FreeEnv eff v)]
  , elements    :: [(Address, TClosure eff eff' v)]
  }

-- discussion: couuld it be modelled differently, for example with higher order effects?
-- separation of effecta (ho effects)