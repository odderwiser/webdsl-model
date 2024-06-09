{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Definitions.Templates.Framework where

import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Templates.Denotation as T
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH)
import Actions.Handlers.Return (funReturn)
import Actions.Framework as A
import Definitions.Templates.Syntax (TemplateDef, tDef)
import Templates.Framework as T
import Templates.Handlers.Env (templatesH)
import qualified Data.Bifunctor
import Data.Bifunctor
--running syntax

-- preprocessing
type Envs = TemplateDef + EntityDef + FDecl
type Eff'' = TDefs Eff Eff' V + EntityDefsEnv Eff V + FunctionEnv Eff V + End
type EnvTy = (FreeEnv Eff V \/ PEnv Eff Eff' V)
type DefSyntax = Envs (Fix Module) \/ Envs Module'

runProgram :: Program (Envs EnvTy) (PEnv Eff Eff' V) -> Out'
runProgram (Fragment defs exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
  $ denoteDefList defs of
  (((_, tEnv), env'), env) -> T.runEnv exp TEnv 
    { actionEnv = Env
      { varEnv = []
      , entityDefs = entityDefs env'
      , defs = U.defs env
      }
    , templates = templates tEnv
    , elements = []
    }

foldTProgram :: (Denote h Eff V, DenoteT f Eff Eff' V, Bifunctor f)
    => Program ((Envs (Fix h)) \/ (Envs (BiFix f (Fix h)))) (BiFix f (Fix h))
    -> Program (Envs EnvTy) (PEnv Eff Eff' V)
foldTProgram (Fragment defs program) = Fragment (map (\e -> case e of
    Left e' -> fmap (Left . foldD) e'
    Right e' -> fmap (Right . foldDT) e') defs) (foldDT program)

instance DenoteDef FDecl EnvTy Eff'' where --- Maybe?? This works???
  denoteDef decl = F.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef EntityDef EnvTy Eff'' where
  denoteDef decl = E.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef TemplateDef EnvTy Eff'' where
  denoteDef decl = T.denoteDefT $ fmap (\(Right d) -> d) decl


tDefEnv a b c = Right $ tDef a b c 