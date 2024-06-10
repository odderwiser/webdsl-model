{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.Pages.Framework where
import Utils as U
import Data.Bifunctor
import Definitions.Program.Syntax
import Definitions.Pages.Syntax
import qualified Definitions.Templates.Framework as T
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Templates.Denotation as T
import Actions.Framework
import Templates.Framework
import Definitions.Pages.Denotation as D (PageDefs, denoteDef)
import Actions.Handlers.Env
import Actions.Handlers.Entity
import Templates.Handlers.Env
import Templates.Framework as T
import Definitions.Fun.Syntax (FDecl)
import Definitions.Entity.Syntax (EntityDef)
import Definitions.Templates.Syntax (TemplateDef)
import Definitions.Templates.Framework (handleDefs)

type Envs = PageDef + T.Envs
type Eff'' = PageDefs Eff Eff' V + T.Eff''
type EnvTy = (FreeEnv Eff V \/ PEnv Eff Eff' V)
type DefSyntax = Envs (Fix Module) \/ Envs Module'

foldProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Functor g)
    => Program (g (Fix h) \/ g (BiFix f (Fix h))) (PageCall (Fix h))
    -> Program (g (FreeEnv eff v \/ PEnv eff eff' v)) (PageCall (FreeEnv eff v))
foldProgram (Fragment defs pg@(PCall name args)) 
    = Fragment (fmap T.foldTDefs defs) (fmap foldD pg)

foldEProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Functor g)
    => Program (g (Fix h) \/ g (BiFix f (Fix h))) ()
    -> Program (g (FreeEnv eff v \/ PEnv eff eff' v)) (PageCall (FreeEnv eff v))
foldEProgram (Program defs) 
    = Fragment (fmap T.foldTDefs defs) (PCall "root" []) -- can root have arguments? 

runProgram :: Program (Envs EnvTy) (PEnv Eff Eff' V) -> Out'
runProgram (Fragment defs exp) = case unwrap
  $ handleDefs
  $ handle_ pagesH (TEnv { pages = []} :: TEnv Eff Eff' V )
  $ denoteDefList defs of
  ((((_, tEnv'), tEnv), env'), env) -> T.runEnv exp 
    $ (T.makeTEnv env' env tEnv) { U.pages = pages tEnv'} 

instance DenoteDef PageDef EnvTy Eff'' where
  denoteDef decl = D.denoteDef $ fmap (\(Right d) -> d) decl

instance DenoteDef FDecl EnvTy Eff'' where --- Maybe?? This works???
  denoteDef decl = F.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef EntityDef EnvTy Eff'' where
  denoteDef decl = E.denoteDef $ fmap (\(Left d) -> d) decl

instance DenoteDef TemplateDef EnvTy Eff'' where
  denoteDef decl = T.denoteDefT $ fmap (\(Right d) -> d) decl
