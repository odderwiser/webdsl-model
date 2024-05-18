{-# OPTIONS_GHC -Wno-missing-fields #-}


module Fun.Denotation where
import Fun.Effects
import Utils.Composition
import Fun.Syntax
import Utils.Free
import Eval.Effects
import Syntax
import Eval.Handlers (environment)
import Utils.Handler (handle_, handle)
import Fun.Handlers
import Utils.Fix
import Utils.Environment (Env (Env), FreeEnv, Function)
import Data.Maybe (mapMaybe)
import Program.Syntax
import Program.Effects
import Utils.Denote
import Entity.Syntax
import qualified Program.Denotation as P

derefDefs :: Functor remEff
    => FunName -> Env remEff v
    -> Free remEff (FDecl (FreeEnv remEff v), Env remEff v)
derefDefs name env = handle_ Fun.Handlers.defs env (deref name)

refDefs :: forall eff fDecl g v. (Functor eff, FDecl <: g, 
  fDecl ~ FDecl (FreeEnv eff v))
  => [g (FreeEnv eff v)] -> Env eff v
  -> Free eff (Env eff v)
refDefs decls env  = do
  (_ :: [FunName], env') <- handle_ Fun.Handlers.defs env $ mapM ref
    $ mapMaybe (\dec -> (proj dec :: Maybe fDecl)) decls
  return env'

-- does this still work with how Variable Declaration is defined?
denote :: (Abort (Fix v) <: eff, MLState Address (Fix v) <: eff, Null <: v)
  => Fun FunName (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (Return e)       env = do
    e' <- e env
    abort e'

denote (FCall name vars) env = do
  (FDecl _ varNames body, _) <- derefDefs name env
  (locs :: [Address])        <- storeVars env vars
  env'                       <- dropEnv env
  env''                      <- refVars varNames locs env' -- this might nor work, are the assignments chained correctyl?
  body env''

dropEnv :: (Functor f') 
  => Env f' v -> Free f' (Env f' v)
dropEnv env = handle dropH $ Fun.Effects.drop env

storeVars env = mapM (\e -> do
  e' <- e env
  ref e')

refVars varNames locs env = do
  (_, env') <- handle_ environment env 
    $ mapM assign 
    $ zip varNames locs
  return env'


denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
  => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
denoteDefs defs = P.denoteDefs Defs defs $ Pure $ Env {}

instance Def FDecl where
    foldDef :: (Def FDecl, Denote f eff v) => FDecl (Fix f) -> FDecl (FreeEnv eff v)
    foldDef decl@(FDecl name vars body) = FDecl name vars $ foldD body