{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE DataKinds #-}
module Actions.Modules.Entity.Denotation where
import Actions.Effects
import Syntax as S
import Utils as U hiding (liftEnv)
import Data.Maybe (mapMaybe, fromJust)
import Actions.Arith as A
import Actions.Bool as B
import Actions.Handlers.Entity
import Actions.Handlers.Env
import qualified Actions.Modules.Fun.Denotation as F
import Definitions.Fun.Syntax (FDecl(FDecl))
import Definitions.Entity.Syntax
import Actions.Modules.Entity.Syntax
import Actions.Modules.Str.Syntax as Str
import Actions.Values as V
import Definitions.GlobalVars.Effects (setEntity, DbWrite, getEntity, DbRead)


denote :: forall e v eff.
  (e ~ FreeEnv eff (Fix v)
  , MLState Address (Fix v) <: eff, EHeap v <: eff
  , DbWrite (Fix v) <: eff
  , Null <: v, Lit Uuid <: v, DbRead (EntityDecl (Fix v)) <: eff
  ) => Entity e -> e
denote (PropAccess object propName) env = do
  obj       <- getObj object env
  getProperty propName obj

denote (PropAssign object propName e)  env = do
  obj :: (EntityDecl (Fix v)) <- getObj object env
  e'                          <- e env
  obj'                        <- setProperty propName e' obj
  uuid :: Uuid                <- ref $ Just obj'
  return V.null

denote (ECall obj fName vars) env = do
  (EDecl name params)   <- getObj obj env
  (EDef _ _ _ funs _)   <- derefH name entityDefsH env
  FDecl _ varNames body <- F.derefDefs fName $ liftDefs funs
  env'                  <- F.populateFunEnv env varNames vars
  locs :: [Address]     <- mapM (ref . snd) params
  env''                 <- refProperties (map fst params) locs env'
  env'''                <- liftEnv env'' $ liftDefs funs
  body env'''

denote (PVar pName) env = do
  loc     <- derefLocalProperty pName env
  deref loc

denote (Save obj) env =  do
  obj <- getObj obj env
  setEntity obj
  return V.null

-- retrieves value of given property by name
getProperty name (EDecl _ params) = return
    $ fromJust
    $ lookup name params

-- replaces property value of a given object
setProperty name v (EDecl eName params) = return 
  $ EDecl eName
  $ map (\(name', v') -> if name == name' then (name, v) else (name', v')) params

-- returns object based on object reference
getObj :: (EHeap v <: f, DbRead (EntityDecl (Fix v)) <: f, Lit Uuid <: v)
  => FreeEnv f (Fix v) -> Env f (Fix v) -> Free f (EntityDecl (Fix v))
getObj object env = do
  id      <- object env
  let uuid = (unbox id :: Uuid)
  entity  <- deref (unbox id :: Uuid)
  loadObj entity (unbox id)

--returns object embedded in 'Fix' based on uuid
getObj' :: (EHeap v <: f, DbRead (EntityDecl (Fix v)) <: f, EntityDecl <: v) 
  => Uuid -> Free f (Fix v)
getObj' uuid = do
  entity  <- deref uuid
  entity' <- loadObj entity uuid
  return $ injF entity'

-- returns object if present, otherwise retrieves it from database (lazy loading)
loadObj :: (MLState Uuid (Maybe (EntityDecl (Fix v))) <: f,  DbRead (EntityDecl (Fix v)) <: f) 
  => Maybe (EntityDecl (Fix v)) -> Uuid -> Free f (EntityDecl (Fix v))
loadObj (Just (e :: EntityDecl (Fix v))) _    = return e
loadObj Nothing                          uuid = do
  entity <- getEntity uuid
  assign (uuid, Just entity)
  return entity

-- lifts object functions into the main environment. todo: could be an effect/ handler
liftDefs defs = Env {U.defs = defs} 

-- dereferences object property (variable) location on heap
derefLocalProperty pName = derefH pName propertyVarEnvH

-- references object properties to the environment
refProperties :: ( Functor eff')
  => [String] -> [Int] -> Env eff v -> Free eff' (Env eff v)
refProperties names locs env = do
  (_, env') <- handle_ propertyVarEnvH env
    $ mapM assign (zip names locs)
  return env'

-- lifts object scope as global scope
liftEnv global obj = handle mutateH $ Actions.Effects.lift global obj

makeObjEnv :: Functor eff => Free eff (Env eff v)
makeObjEnv = handle mutateH genEnv

-- extractDefaultValues :: forall f' v a. (LitInt <: v,
--  LitBool <: v, [] <: v,  Null <: v, Functor f')
--   => [(a, Type)] -> Free f' [Fix v]
-- extractDefaultValues = mapM ((\param -> do
--   handle defaultTypeH $ defaultType param) . snd)

-- populates environment with default values for undefined variables
populateObjEnv objEnv defaultEnv = handle mutateH
  $ populateMissingDefault objEnv defaultEnv


denoteEDecl :: forall eff v.
  ( MLState Address (Fix v) <: eff, EHeap v <: eff, Random String String <: eff
  , Lit Address <: v, Lit Uuid <: v, LitStr <: v, Null <: v
  , Show (v (Fix v))
  ) => EntityDecl (FreeEnv eff (Fix v))
    -> FreeEnv eff (Fix v)
denoteEDecl decl@(EDecl entity props) env = do
  def@(EDef
    name propsDefs iProps funs _) <- derefH entity entityDefsH env
  values                        <- mapM ((\e -> e env) . snd) props
  implProps                     <- mapM (denoteImplicitProps (name, values)) iProps
  id :: Uuid                    <- ref . Just $ mapProperties decl values implProps
  return $ box id

denoteEDecl' :: forall eff v.
  ( MLState Address (Fix v) <: eff, Random String String <: eff
  , Lit Address <: v, Lit Uuid <: v, LitStr <: v, Null <: v
  , Show (v (Fix v)), EntityDecl <: v
  ) => EntityDecl (FreeEnv eff (Fix v))
    -> Env eff (Fix v) -> Free eff (Fix v)
denoteEDecl' decl@(EDecl entity props) env = do
  def@(EDef
    name propsDefs iProps funs _) <- derefH entity entityDefsH env
  values                        <- mapM ((\e -> e env) . snd) props
  implProps                     <- mapM (denoteImplicitProps (name, values)) iProps
  return $ injF $ mapProperties decl values implProps

-- maps property definitions to values
mapProperties (EDecl entity props) vals implProps =
  EDecl entity
  $ implProps
  ++ zipWith
    (curry (\((name, ty), v) -> (name, v)))
    props vals

-- ascribes values to properties that have an implicit definition
denoteImplicitProps :: forall f e v.
  (Show e,Random String String <: f, Lit Uuid <: v
  ) => e -> ImplicitProp -> Free f (PName, Fix v)
denoteImplicitProps def Id = do
  uuid :: Uuid <- random def
  return ("id", box uuid)