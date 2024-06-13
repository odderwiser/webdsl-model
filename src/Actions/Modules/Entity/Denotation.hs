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
import Definitions.GlobalVars.Syntax (Uuid)

getProperty name (EDecl _ params) = return
      $ fromJust $ lookup name params

getObj :: (EHeap v <: f, LitV Uuid <: v)
  => FreeEnv f (Fix v) -> Env f (Fix v) -> Free f (EntityDecl (Fix v))
getObj object env = do
  id      <- object env
  deref 
    (fromJust $ unbox id :: Uuid)

-- derefObj obj env = derefH (getAddress obj) heap'' (entities env)
liftDefs defs = Env {U.defs = defs}

denote :: forall e v eff.
  (e ~ FreeEnv eff (Fix v)
  , MLState Address (Fix v) <: eff, EHeap v <: eff
  , Null <: v, LitV Uuid <: v
  ) => Entity e -> e
denote (PropAccess object propName) env = do
  obj       <- getObj object env
  getProperty propName obj

-- except that is not what really happens
-- there is some flushing semantics that need to happen fist
denote (PropAssign object propName e)  env = do
  obj :: (EntityDecl (Fix v)) <- getObj object env
  loc              <- getProperty propName  obj
  e'               <- e env
  return S.null

denote (ECall obj fname vars) env = do
  (EDecl name params)   <- getObj obj env
  (EDef _ _ _ funs)     <- derefH name entityDefsH env
  FDecl _ varNames body <- F.derefDefs fname $ liftDefs funs
  env'                  <- F.populateFunEnv env varNames vars
  locs :: [Address]     <- mapM (ref . snd) params
  env''                 <- refProperties (map fst params) locs env'-- possibly different orfder? check
  env'''                <- liftEnv env'' $ liftDefs funs
  body env'''

denote (PVar pname) env = do
  loc     <- derefLocalProperty pname env
  deref loc

derefLocalProperty pname = derefH pname propertyVarEnvH

refProperties names locs env = do
  (_, env') <- handle_ propertyVarEnvH env
    $ mapM assign (zip names locs)
  return env'

liftEnv global obj = handle mutateH $ Actions.Effects.lift global obj

makeObjEnv :: Functor eff => Free eff (Env eff v)
makeObjEnv = handle mutateH genEnv

extractDefaultValues :: forall f' v a. (LitInt <: v,
 LitBool <: v, [] <: v,  Null <: v, Functor f')
  => [(a, Type)] -> Free f' [Fix v]
extractDefaultValues = mapM ((\param -> do
  handle defaultTypeH $ defaultType param) . snd)

populateObjEnv objEnv defaultEnv = handle mutateH
  $ populateMissingDefault objEnv defaultEnv

mapProperties (EDecl entity props) vals implProps =
  EDecl entity
  $ implProps
  ++ zipWith (curry (\((name, ty), v) -> (name, v)))
    props vals


denoteEDecl :: forall eff v.
  ( MLState Address (Fix v) <: eff, EHeap v <: eff, Random String String <: eff
  , LitV Address <: v, LitV Uuid <: v, LitStr <: v, Null <: v
  , Show (v (Fix v))
  ) => EntityDecl (FreeEnv eff (Fix v))
    -> FreeEnv eff (Fix v)
denoteEDecl decl@(EDecl entity props) env = do
  def@(EDef 
    name propsDefs iProps funs) <- derefH entity entityDefsH env
  values                        <- mapM ((\e -> e env) . snd) props
  implProps                     <- mapM (denoteImplicitProps (name, values)) iProps -- lousy id but will do for now??
  id :: Uuid                    <- ref $ mapProperties decl values implProps
  return $ box id

denoteImplicitProps :: forall f e v. 
  (Show e,Random String String <: f, LitV Uuid <: v
  ) => e -> ImplicitProp -> Free f (PName, Fix v)
denoteImplicitProps def Id = do
  uuid :: Uuid <- random def
  return ("id", box uuid)