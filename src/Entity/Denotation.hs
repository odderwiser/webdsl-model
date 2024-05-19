{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE DataKinds #-}
module Entity.Denotation where
import Eval.Effects
import Syntax as S
import Utils.Fix
import Utils.Composition
import Eval.Syntax
import Utils.Denote
import Fun.Denotation (derefDefs)
import Fun.Handlers
import Eval.Denotation (derefEnv, refEnv)
import Entity.Syntax as E
import Utils.Free
import Utils.Handler
import Utils.Environment as E
import Fun.Syntax
import Program.Effects
import Data.IntMap (mapMaybe)
import Entity.Handlers as H
import Program.Denotation as P
import Fun.Denotation as F
import Eval.Handlers (heap'', environment, heap')
import Fun.Effects as F
import Entity.Effects
import Arith.Syntax
import Bool.Syntax

getProperty name = derefH name objEnvH

-- derefObj obj env = derefH (getAddress obj) heap'' (entities env)
liftDefs defs = Env {E.defs = defs}

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
denote (E.FCall obj fname vars) env = do
  obj'                <- obj env
  -- case projEntity obj' of
  --   (EDecl name params) -> do
  (EDef _ _ funs)       <- derefH (projEName obj') entityDefsH env
  FDecl _ varNames body <- derefDefs fname $ liftDefs funs
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

-- makeDefaultEnv :: forall eff v.
--   (LitInt <: v,
--   LitBool <: v, [] <: v,  Null <: v,
--   MLState Address (Fix v) <: eff) 
--   => EntityDef (FreeEnv eff (Fix v)) 
--   -> Free eff (Env eff (Fix v))
-- makeDefaultEnv (EDef name params funs) = do
--   objEnv               <- makeObjEnv
--   (values :: [Fix v])  <- extractDefaultValues params
--   locs                 <- mapM ref values
--   objEnv'              <- refVars (map fst params) locs objEnv
--   refDefs funs objEnv'

extractDefaultValues :: forall f' v a. (LitInt <: v,
 LitBool <: v, [] <: v,  Null <: v, Functor f')
  => [(a, Type)] -> Free f' [Fix v]
extractDefaultValues = mapM ((\param -> do
  handle defaultTypeH $ defaultType param) . snd)

-- populateObjEnv :: Functor eff => Free eff (Env eff v)
populateObjEnv objEnv defaultEnv = handle mutateH
  $ populateMissingDefault objEnv defaultEnv

-- denoteEvalEDecl :: Eval VName (EntityDecl (FreeEnv eff (Fix v))) -> Env eff (Fix v) -> Free eff b
-- denoteEvalEDecl (VValDecl name (EDecl entity props) k) env = do
--   entityDef   <- derefH entity entityDefsH env
--   locs        <- F.storeVars env (map snd props)
--   objEnv       <- createObjectEnv entity props entityDef locs
--   (loc, env'') <- refH (entity, objEnv) entityDeclsH env
--   loc'         <- ref (injF $ Box loc)
--   env''        <- refEnv name loc' env
--   k $ env''

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

denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
  => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
denoteDefs defs = P.denoteDefs Defs defs
  $ P.denoteDefs Entities defs $ Pure Env {}

instance Def EntityDef where
  foldDef :: (Def EntityDef, Denote f eff v)
    => EntityDef (Fix f) -> EntityDef (FreeEnv eff v)
  foldDef (EDef name props funs) = EDef name props
    $ map foldDef funs
