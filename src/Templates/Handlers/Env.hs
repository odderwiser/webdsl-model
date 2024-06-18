module Templates.Handlers.Env where
import Actions.Effects (MLState (..))
import Templates.Modules.Attributes.Syntax (AttName)
import Utils as U
import Actions.Handlers.Env (mkAHandler, mkRHandler)
import Definitions.Templates.Syntax (TName, TemplateDef (TDef))
import Syntax (Type, Address)
import Data.List (find)
import Actions.Syntax
import Data.Maybe (fromJust)
import Definitions.Pages.Syntax (PgName, PageDef (..))
import qualified Data.Map as Map
import Templates.Effects (ReqParamsSt)
import Data.IntMap (findWithDefault)
import Definitions.Templates.Denotation (TDefs)

type AttsEnv = MLState AttName String

attsH :: (Functor eff, Functor eff')
  => Handler_ AttsEnv
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v)
attsH = mkAHandler U.attributes
   lookup (\record env -> env {U.attributes = record : U.attributes env})

type TempEnv eff eff' v = MLState (TName, [Type]) (TemplateDef (PEnv eff eff' v) (Env eff v) )

templatesH :: (Functor eff, Functor eff')
  => Handler_ (TDefs eff eff'' v)
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v)
templatesH = mkRHandler U.templates
  (\ key env -> find (\val -> mapToKey val == key ) $ env)
  (\ k record env -> k (mapToKey record)
    $  env {U.templates = record : U.templates env})

mapToKey (TDef name args _) = (name, map snd args)

type PgEnv eff eff' v = MLState PgName (PageDef (PEnv eff eff' v) (FreeEnv eff v) )

pagesH :: (Functor eff, Functor eff')
  => Handler_ (PgEnv eff eff'' v)
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v)
pagesH = mkRHandler U.pages
  (\ key env -> find (\val -> mapToPgName val == key) $ env)
  (\ k record env -> k (mapToPgName record)
    $  env {U.pages = record : U.pages env})

mapToPgName (PDef name _ _ ) = name

type ElemEnv eff eff' v = MLState Address (TEnv eff eff' v, PEnv eff eff' v)

elementsH :: (Functor eff, Functor eff')
  => Handler_ (ElemEnv eff eff'' v)
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v)
elementsH = mkRHandler U.elements
  lookup
  (\k value env ->
    let key = length $ U.elements env
    in
      k key $ env {U.elements = (key, value) : U.elements env})

paramsH :: (Functor eff) => Handler_ ReqParamsSt a (Map.Map String String) eff a
paramsH = Handler_
  { ret_ = \v map -> pure v
  , hdlr_ = \op map -> case op of
      (Assign v  k) -> k $ uncurry Map.insert v map
      (Deref  v k) -> k (Map.findWithDefault "" v map) map
  }

mkParamsMap = Map.empty