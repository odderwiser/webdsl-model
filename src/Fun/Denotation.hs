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
import Utils.Environment (Env (Env), FreeEnv, Function, derefH)
import Data.Maybe (mapMaybe)
import Program.Syntax
import Program.Effects
import Utils.Denote
import Program.Denotation as P
import Eval.Syntax

derefDefs :: Functor eff => FunName -> Env eff (Fix v) 
  -> Free eff (FDecl (FreeEnv eff (Fix v)))
derefDefs name env = derefH name Fun.Handlers.defs env 

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
  => Fun (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (Return e)       env = do
    e' <- e env
    abort e'

denote (FCall name vars) env = do
  FDecl _ varNames body <- derefDefs name env
  env'                  <- populateEnv env varNames vars
  body env'

populateEnv :: (MLState Address v <: f) 
  => Env f v 
  -> [VName] -> [FreeEnv f v] -> Free f (Env f v)
populateEnv env varNames vars = do
  locs       <- storeVars env vars
  env'       <- dropEnv env
  refVars (zip varNames locs) env'

dropEnv :: (Functor f') 
  => Env f' v -> Free f' (Env f' v)
dropEnv env = handle dropH $ Fun.Effects.drop env

storeVars :: (MLState Address v <: f) 
  => Env f v -> [FreeEnv f v] -> Free f [Address]
storeVars env = mapM (\e -> do
  e' <- e env
  ref e') 

refVars :: (Functor eff,  
  MLState Address v <: eff) 
  => [(VName, Address)] -> Env eff v -> Free eff (Env eff v)
refVars envTuples env = do
  (_, env') <- handle_ environment env 
    $ mapM assign envTuples
  return env'


denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
  => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
denoteDefs defs = P.denoteDefs Defs defs $ Pure $ Env {}

instance Def FDecl where
    foldDef :: (Def FDecl, Denote f eff v) => FDecl (Fix f) -> FDecl (FreeEnv eff v)
    foldDef decl@(FDecl name vars body) = FDecl name vars $ foldD body