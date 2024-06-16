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
  $ handleDefs 
  $ denoteDefList defs of
  (((_, tEnv), env'), env) -> T.runEnv exp 
    $ makeTEnv env' env tEnv

handleDefs defs = handle_ 
  defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V )
  $ handle_ templatesH (TEnv { templates = []} :: TEnv Eff Eff' V )
  defs

makeTEnv :: Env eff v -> Env eff v -> TEnv eff eff' v -> TEnv eff eff' v
makeTEnv eEnv fEnv tEnv = TEnv 
    { actionEnv = Env
      { varEnv = []
      , entityDefs = entityDefs eEnv
      , defs = U.defs  fEnv
      , globalVars = []
      }
    , templates = templates tEnv
    , elements = []
    }

foldTProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Functor g)
    => Program (g (Fix h) \/ g (BiFix f (Fix h))) (BiFix f (Fix h))
    -> Program (g (FreeEnv eff v \/ PEnv eff eff' v)) (PEnv eff eff' v)
foldTProgram (Fragment defs program) = Fragment (fmap foldTDefs defs) (foldDT program)

foldTDefs :: (Functor g, Denote h eff v, DenoteT f eff eff' v, Bifunctor f) 
  => g (Fix h) \/ g (BiFix f (Fix h)) -> g (FreeEnv eff v \/ PEnv eff eff' v)
foldTDefs (Left e) = fmap (Left . foldD) e
foldTDefs (Right e) = fmap (Right . foldDT) e

instance DenoteDef FDecl EnvTy Eff'' where --- Maybe?? This works???
  denoteDef decl = F.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef EntityDef EnvTy Eff'' where
  denoteDef decl = E.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef TemplateDef EnvTy Eff'' where
  denoteDef decl = T.denoteDefT $ fmap (\(Right d) -> d) decl


tDefEnv a b c = Right $ tDef a b c 