module Templates.Handlers.Env where
import Actions.Effects (MLState (..))
import Templates.Modules.Attributes.Syntax (AttName)
import Utils as U
import Actions.Handlers.Env (mkAHandler)

type AttsEnv = MLState AttName String

attsH :: (Functor eff, Functor eff')
  => Handler_ AttsEnv
  a (TEnv eff eff'' v) eff' (a, TEnv eff eff'' v) 
attsH = mkAHandler U.attributes
   lookup (\record env -> env {U.attributes = record : (U.attributes env)})
