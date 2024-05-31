{-# OPTIONS_GHC -Wno-missing-fields #-}


module Actions.Modules.Fun.Denotation where
import Actions.Effects as E
import Utils.Composition
import Actions.Modules.Fun.Syntax
import Utils
import Syntax
import Actions.Handlers.Heap (environment)
import Actions.Handlers.Env
import Data.Maybe (mapMaybe)
import Program.Syntax
import Program.Effects
import Program.Denotation as P
import Actions.Modules.Eval.Syntax

derefDefs :: Functor eff => FunName -> Env eff (Fix v) 
  -> Free eff (FDecl (FreeEnv eff (Fix v)))
derefDefs name env = derefH name defsH env 

refDefs :: forall eff fDecl g v. (Functor eff, FDecl <: g, 
  fDecl ~ FDecl (FreeEnv eff v))
  => [g (FreeEnv eff v)] -> Env eff v
  -> Free eff (Env eff v)
refDefs decls env  = do
  (_ :: [FunName], env') <- handle_ defsH env $ mapM ref
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
dropEnv env = handle dropH $ E.drop env

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

denoteDef :: (MLState FunName (Function eff v) <: eff') 
  => FDecl (FreeEnv eff v) -> Free eff' ()
denoteDef decl@(FDecl name _ _) = do
  (name :: FunName) <- ref decl
  return ()

denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
  => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
denoteDefs defs = P.denoteDefs Defs defs $ Pure $ Env {}