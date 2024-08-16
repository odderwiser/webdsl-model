module Definitions.GlobalVars.Denotation where
import Actions.Effects
import Syntax as S
import Actions.Syntax
import Definitions.GlobalVars.Syntax
import Utils.Environment (FreeEnv)
import Utils  as U hiding (denote)
import Actions.Modules.Entity.Denotation (denoteEDecl, denoteEDecl')
import Actions.Modules.Str.Syntax (LitStr)
import Data.Traversable
import Control.Monad (foldM)
import Definitions.Entity.Denotation
import Definitions.GlobalVars.Effects (connect, loadVars, getEntity, DbRead, setVar, setEntity, DbWrite, DbWrite')
import Actions.Modules.Eval.Denotation (refEnv, derefEnv, refEnv', derefEnv')
import Data.Maybe (fromJust)
import Actions.Values
import Actions.Handlers.Entity hiding (updateEntity)
import Templates.Modules.Lift.Syntax
import Actions.Handlers.Heap (global)
import qualified Actions.Values as V
import Data.Bifunctor
import Actions.Handlers.Env (derefH)
import Definitions.Entity.Syntax (EntityDef(EDef))
import Definitions.Syntax (PName)

type VarEnv = MLState VName Address
type Heap v = MLState Address (Fix v)

refGlobalEnv :: (Functor eff, Functor eff')
  => VName -> Address
  -> Env eff v -> Free eff' (Env eff v)
refGlobalEnv name loc env = do
  (_, env') <- handle_ global env (assign (name, loc))
  return env'

denote :: forall eff v.
  ( DbRead (EntityDecl (Fix v)) <: eff, Cond <: eff
  , Heap v <: eff, DbWrite  (Fix v) <: eff
  , Denote EntityDecl eff (Fix v)
  , Lit Uuid <: v, Lit Address <: v, EntityDecl <: v
  , LitStr <: v, Null <: v, [] <: v
  , Random String String <: eff
  , Writer (VName, Address) <: eff
  , Show (v (Fix v)), TempEHeap v <: eff
  ) => VarList (FreeEnv eff (Fix v)) ->  Env eff (Fix v) -> Free eff (Fix v)
denote (VList list) env = do
  isSuccess <- connect (Nothing :: MaybeEntity v)
  env' <- cond' isSuccess
    (loadVariables env)
    (evaluateVariables list env id)
  mapM_ write $ globalVars env'
  return V.null

-- denoteWeaken :: forall f g v.
--   ( Denote EntityDecl f (Fix v)
--   , MLState Address (Fix v) <: f
--   , Random String String <: f, [] <: v
--   , DbRead (EntityDecl (Fix v)) <: f
--   ,  DbWrite (Fix v) <: f
--   , Lit Uuid <: v,  Lit Address <: v
--   , TempEHeap v <: f
--   , Cond <: f, EntityDecl <: v, Null <: v,  Show (v (Fix v))
--   ) => [GlobalVar (FreeEnv f (Fix v))]
--     -> Env f (Fix v) -> Free f (Env f (Fix v))
-- denoteWeaken list env =  do
--   isSuccess <- connect (Nothing :: MaybeEntity v)
--   cond' isSuccess
--     (loadVariables env)
--     (evaluateVariables list env id)

loadVariables :: forall eff v.
  ( MLState Address (Fix v) <: eff, Functor eff
  ,  DbRead (EntityDecl (Fix v)) <: eff
  ,  Lit Uuid <: v
  ) => Env eff (Fix v) -> Free eff (Env eff (Fix v))
loadVariables env = do
  vars <- loadVars (Nothing :: MaybeEntity v)
  foldM loadVariable env vars

loadVariable :: forall f v. (MLState Address (Fix v) <: f, Lit Uuid <: v) 
  => Env f (Fix v) -> (VName, Uuid) -> Free f (Env f (Fix v))
loadVariable env (name, id) = do
  (loc :: Address)  <- ref (box id :: Fix v)
  refGlobalEnv name loc env

evaluateVariables ::
  (Functor eff, Heap v <: eff
  , Lit Address <: v, Lit Uuid <: v, EntityDecl <: v
  , Denote EntityDecl eff (Fix v)
  , LitStr <: v, Null <: v, [] <: v
  , Random String String <: eff
  , Show (v (Fix v))
  , Heap v <: eff
  , DbWrite (Fix v) <: eff, TempEHeap v <: eff
  ) => [GlobalVar (FreeEnv eff (Fix v))] -> Env eff (Fix v)
    -> (Free eff (Fix v) -> Free eff (Fix v))
    -> Free eff (Env eff (Fix v))
evaluateVariables list env lift = do
  env' <- foldM refObjNames env (getNames list)
  mapM_ (refObjects env') list
  mapM_ (updateRefs env') (getNames list)
  mapM_ (writeVars env') list
  return env'

refObjNames :: forall eff v.
  ( Functor eff, Heap v <: eff, TempEHeap v <: eff, Null <:v
  , Lit Address <: v
  ) => Env eff (Fix v) -> VName
    -> Free eff (Env eff (Fix v))
refObjNames env name = do
  (loc  :: Address)   <- ref (Nothing :: MaybeEntity v)
  (loc' :: Address)   <- ref (box loc :: Fix v)
  refGlobalEnv name loc' env

refObjects :: forall eff v.
  ( Functor eff, Heap v <: eff
  , Denote EntityDecl eff (Fix v)
  , Lit Address <: v, EntityDecl <: v, LitStr <: v, Null <: v
  , Random String String <: eff, TempEHeap v <: eff
  , Show (v (Fix v))
  , Heap v <: eff
  ) => Env eff (Fix v)
    -> GlobalVar (FreeEnv eff (Fix v))
    -> Free eff ()
refObjects env (VDef name entity) = do
  loc              <- derefEnv' name env
  (box' :: Fix v)  <- deref loc
  entity'          <- denoteEDecl' entity (env :: Env eff (Fix v))
  assign
    ( unbox box' :: Address
    , Just $ projEntity entity')

-- only now we have a guarantee that all objects have SOME uuid. updates refs between objects
updateRefs :: forall eff v.
  ( Functor eff, Heap v <: eff, [] <: v
  , DbWrite (Fix v) <: eff, EntityDecl <: v, TempEHeap v <: eff
  , Lit Address <: v, Lit Uuid <: v
  ) => Env eff (Fix v) -> VName -> Free eff ()
updateRefs env name = do
  loc                       <- derefEnv' name env
  (box' :: Fix v)           <- deref loc
  (entity :: MaybeEntity v) <- deref (unbox box' :: Address)
  (uuid :: Uuid)            <- updateEntity entity env
  assign
    ( loc
    , box uuid :: Fix v )

updateEntity :: forall f v.
  ( Heap v <: f, EntityDecl <: v
  , Lit Uuid <: v,  Lit Address <: v, TempEHeap v <: f, [] <: v
  , DbWrite (Fix v) <: f
  ) => Maybe (EntityDecl (Fix v)) -> Env f (Fix v) -> Free f Uuid
updateEntity (Just e@(EDecl name params)) env = do
  (EDef name paramsTy _ _ _) <- derefH name entityDefsH env
  params'                    <- mapM (mapParams paramsTy) params 
  setEntity (EDecl name params')
  return $ getUuid e

mapParams :: forall f v.
  ( Lit Uuid <: v,  Lit Uuid <: v, Lit Address <: v, [] <: v
  , EntityDecl <: v, Functor f, TempEHeap v <: f
  ) => [(PName, Type)] ->  (PName, Fix v) -> Free f (PName, Fix v)
mapParams types (name, value) = case lookup name types of
  Just (Entity e) -> case projF value of
    (Just (Box (address :: Address))) -> do 
      ref <- mapObjectReference address
      return (name, ref)
    _ -> return (name, value)
  Just (List ty') -> do
    list <- mapM (\v -> mapParamsValues (v, ty')) $ projC value
    return (name, injF list)
  _ -> return (name, value)

mapParamsValues (value, Entity e) = mapParamsAddress (projF value) value

mapParamsValues (value, List ty) = do
  list <- mapM (\v -> mapParamsValues (v, ty)) $ projC value
  return $ injF list
mapParamsValues (value, _) = return value

mapParamsAddress (Just (Box (address :: Address))) _     = mapObjectReference address
mapParamsAddress Nothing                           value = return value

mapObjectReference :: forall f v. (TempEHeap v <: f, Lit Uuid <: v) => Address -> Free f (Fix v)
mapObjectReference address = do
    (entity :: MaybeEntity v) <- deref address
    return $ box $ getUuid $ fromJust entity

-- writes variables into the database
writeVars :: forall f g v. (Heap v <: g, Functor f
  , DbWrite (Fix v) <: g
  , Lit Uuid <: v
  ) => Env f (Fix v) -> GlobalVar (FreeEnv f (Fix v))
  -> Free g ()
writeVars env (VDef name _) = do
  loc            <- derefEnv' name env
  (id :: Fix v)  <- deref loc
  setVar
    ( Nothing :: Maybe (Fix v) )
      name
    ( unbox id :: Uuid )


