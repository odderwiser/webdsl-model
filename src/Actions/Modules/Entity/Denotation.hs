{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE DataKinds #-}
module Actions.Modules.Entity.Denotation where
import Actions.Effects
import Syntax as S
import Utils as U hiding (liftEnv)
import Data.Maybe (mapMaybe)
import Actions.Arith as A
import Actions.Bool as B
import Actions.Handlers.Entity
import Actions.Handlers.Env
import qualified Actions.Modules.Fun.Denotation as F
import Definitions.Fun.Syntax (FDecl(FDecl))
import Definitions.Entity.Syntax
import Actions.Modules.Entity.Syntax
import Actions.Modules.Str.Syntax as Str

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

denote (ECall obj fname vars) env = do
  obj'                <- obj env
  (EDef _ _ _ funs)       <- derefH (projEName obj') entityDefsH env
  FDecl _ varNames body <- F.derefDefs fname $ liftDefs funs
  env'                  <- F.populateFunEnv env varNames vars
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

mapProperties (EDecl entity props) locs implProps =
  EDecl entity
  $ map      (\(name, loc) -> (name, injF $ Box loc)) implProps 
  ++ zipWith (curry (\((name, ty), loc) -> (name, injF $ Box loc)))
    props locs


denoteEDecl :: forall eff v.
  (Functor eff,
  MLState Address (Fix v) <: eff,
  EntityDecl <: v, LitAddress <: v,
  Random String <: eff, LitStr <: v, Null <: v)
  => EntityDecl (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denoteEDecl decl@(EDecl entity props) env = do
  (EDef name propsDefs iProps funs) <- derefH entity entityDefsH env
  locs                              <- F.storeVars env id (map snd props)
  implProps                         <- mapM (denoteImplicitProps (injF Null :: Fix v)) iProps
  return 
    $ injF $ mapProperties decl locs implProps

denoteImplicitProps :: forall f v. (MLState Address (Fix v) <: f,
  Random String <: f, LitStr <: v) => Fix v -> ImplicitProp -> Free f (PName, Address)
denoteImplicitProps _ Id = do
  uuid :: String <- random
  loc            <- ref (Str.lit uuid :: Fix v)
  return ("id", loc)