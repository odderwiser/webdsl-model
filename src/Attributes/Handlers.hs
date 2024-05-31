module Attributes.Handlers where
import Attributes.Syntax (AttName)
import Utils.Handler
import Utils.Environment
import qualified Utils.Environment as U

-- type AttsEnv = MLState AttName String

-- attsH :: (Functor eff, Functor eff')
--   => Handler_ AttsEnv
--   a (Env eff v) eff' (a, Env eff v)
-- attsH = mkAHandler U.attributes
--   lookup (\record env -> env {U.attributes = record : (U.attributes env)})