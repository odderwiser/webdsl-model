{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE DataKinds #-}
module Entity.Denotation where
import Actions.Effects
import Syntax as S
import Utils as U
import Data.Maybe (mapMaybe)
import Entity.Syntax
import Actions.Syntax
import Actions.Arith
import Actions.Bool
import Entity.Handlers
import Actions.Handlers.Env
import qualified Actions.Modules.Fun.Denotation as F
import Entity.Effects

getProperty name = derefH name objEnvH

-- derefObj obj env = derefH (getAddress obj) heap'' (entities env)
liftDefs defs = Env {U.defs = defs}

denote :: forall e v eff.
  (e ~ FreeEnv eff (Fix v),
    MLState Address (Fix v) <: eff,
    Null <: v, EntityDecl <: v, LitAddress <: v)
  => Entity e -> e
denote (PropAccess object propName) env = do
  objEnv    <- object env
  loc       <- getProperty propName $ projParams objEnv
  deref loc

-- except that is not what really happens
-- there is some flushing semantics that need to happen fist
denote (PropAssign object propName e)  env = do
  objEnv           <- object env
  loc              <- getProperty propName $ projParams objEnv
  e'               <- e env
  assign (loc, e')
  return S.null

-- denoteFCall :: (e ~ FreeEnv eff (Fix v),
--   MLState Address (Fix v) <: eff,
--   Null <: v, EntityDecl <: v, LitAddress <: v)
--   => Fun (e, FunName) e -> e
denote (ECall obj fname vars) env = do
  obj'                <- obj env
  -- case projEntity obj' of
  --   (EDecl name params) -> do
  (EDef _ _ funs)       <- derefH (projEName obj') entityDefsH env
  FDecl _ varNames body <- F.derefDefs fname $ liftDefs funs
  env'                  <- F.populateEnv env varNames vars
  env''                 <- refProperties (projParams obj') env'-- possibly different orfder? check
  env'''                <- liftEnv env'' $ liftDefs funs
  body env'''

denote (PVar pname) env = do
  loc     <- derefLocalProperty pname env 
  deref loc

derefLocalProperty pname = derefH pname propertyVarEnvH

refProperties envTuples env = do
  (_, env') <- handle_ propertyVarEnvH env 
    $ mapM assign envTuples
  return env'

liftEnv global obj = handle mutateH $ lift global obj

makeObjEnv :: Functor eff => Free eff (Env eff v)
makeObjEnv = handle mutateH genEnv

extractDefaultValues :: forall f' v a. (LitInt <: v,
 LitBool <: v, [] <: v,  Null <: v, Functor f')
  => [(a, Type)] -> Free f' [Fix v]
extractDefaultValues = mapM ((\param -> do
  handle defaultTypeH $ defaultType param) . snd)

-- populateObjEnv :: Functor eff => Free eff (Env eff v)
populateObjEnv objEnv defaultEnv = handle mutateH
  $ populateMissingDefault objEnv defaultEnv

mapProperties (EDecl entity props) locs = 
  EDecl entity 
  $ zipWith 
    (curry (\((a, b), c) -> (a, injF $ Box c))) 
    props locs  

denoteEDecl :: forall eff v.
  (Functor eff, LitAddress <: v,
  MLState Address (Fix v) <: eff,
  EntityDecl <: v)
  => EntityDecl (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denoteEDecl decl@(EDecl entity props) env = do
  entityDef      <- derefH entity entityDefsH env
  locs           <- F.storeVars env (map snd props)
  return $ injF 
    $ mapProperties decl locs

-- denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
--   => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
-- denoteDefs defs = P.denoteDefs Entities defs
--   $ F.denoteDefs defs
  
denoteDef :: (EntityDefsEnv eff v <: f) 
  => EntityDef (FreeEnv eff v) -> Free f ()
denoteDef entity@(EDef name _ _) = do
  (name :: EName) <- ref entity 
  return ()  

-- refEntities :: forall eff g v eDef. (Functor eff, EntityDef <: g,
--     eDef ~ EntityDef (FreeEnv eff v))
--     => [g (FreeEnv eff v)] -> Env eff v
--     -> Free eff (Env eff v)
-- refEntities entities env  = do
--   (_ :: [EName], env') <- handle_ entityDefsH env 
--     $ mapM ref
--     $ mapMaybe (\dec -> (proj dec :: Maybe eDef)) entities
--   return env'
