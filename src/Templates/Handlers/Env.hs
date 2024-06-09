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

type AttsEnv = MLState AttName String

attsH :: (Functor eff, Functor eff')
  => Handler_ AttsEnv
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v)
attsH = mkAHandler U.attributes
   lookup (\record env -> env {U.attributes = record : U.attributes env})

type TempEnv eff eff' v = MLState (TName, [Type]) (TemplateDef (PEnv eff eff' v))

templatesH :: (Functor eff, Functor eff')
  => Handler_ (TempEnv eff eff'' v)
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v)
templatesH = mkRHandler U.templates
  (\ key env -> find (\val -> mapToKey val == key ) $ env)
  (\ k record env -> k (mapToKey record) $  env {U.templates = record : U.templates env})

mapToKey (TDef name params body) = (name,map snd params)

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
