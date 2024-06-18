module Definitions.GlobalVars.Denotation where
import Actions.Effects
import Syntax as S
import Actions.Syntax
import Definitions.GlobalVars.Syntax
import Utils.Environment (FreeEnv)
import Utils  as U
import Actions.Modules.Entity.Denotation (denoteEDecl, denoteEDecl')
import Actions.Modules.Str.Syntax (LitStr)
import Data.Traversable
import Control.Monad (foldM)
import Definitions.Entity.Denotation
import Definitions.GlobalVars.Effects (connect, loadVars, getEntity, DbRead, setVar, setEntity, DbWrite)
import Actions.Modules.Eval.Denotation (refEnv, derefEnv, refEnv', derefEnv')
import Data.Maybe (fromJust)
import Actions.Values
import Actions.Handlers.Entity hiding (updateEntity)
import Templates.Modules.Lift.Syntax
import Actions.Handlers.Heap (global)

type VarEnv = MLState VName Address
type Heap v = MLState Address (Fix v)

refGlobalEnv :: (Functor eff, Functor eff')
  => VName -> Address 
  -> Env eff v -> Free eff' (Env eff v)
refGlobalEnv name loc env = do
  (_, env') <- handle_ global env (assign (name, loc))
  return env' 

denoteT :: forall f v g v'. ( Denote EntityDecl f (Fix v)
  , EHeap v <: g,  Heap v <: g, Heap v <: f
  , TempEHeap v <: f, TempEHeap v <: g,  Random String String <: f
  , DbRead (EntityDecl (Fix v)) <: g
  ,  DbWrite (EntityDecl (Fix v)) <: f, DbWrite (EntityDecl (Fix v)) <: g
  , Lit Uuid <: v,  Lit Address <: v
  , EntityDecl <: v, Null <: v,  Show (v (Fix v)), v' ~ Fix v
  , Functor g, Lift f g (Fix v)
  ) => VarListT  (PEnv f g v') (FreeEnv f v') -> PEnv f g v'
denoteT (VList list t) env = do
  env' <- denoteWeaken' list (actionEnv env :: Env f v') U.lift
  t env {actionEnv = env'}

denoteWeaken' :: forall f g v.
  ( Denote EntityDecl f (Fix v), Functor g
  , EHeap v <: g,  Heap v <: g, Heap v <: f
  , TempEHeap v <: f, TempEHeap v <: g, Random String String <: f
  , DbRead (EntityDecl (Fix v)) <: g
  ,  DbWrite (EntityDecl (Fix v)) <: g
  , Lit Uuid <: v,  Lit Address <: v
  , EntityDecl <: v, Null <: v,  Show (v (Fix v))
  ) => [GlobalVar (Env f (Fix v) -> Free f (Fix v))]
    -> Env f (Fix v) -> (Free f (Fix v) -> Free g (Fix v))
    -> Free g (Env f (Fix v))
denoteWeaken' list env lift = do
  isSuccess <- connect (Nothing :: MaybeEntity v)
  if isSuccess
    then loadVariables env
    else evaluateVariables list env lift

denote :: forall eff v.
  ( DbRead (EntityDecl (Fix v)) <: eff, Cond <: eff, EHeap v <: eff
  , Heap v <: eff, TempEHeap v <: eff, DbWrite (EntityDecl (Fix v)) <: eff
  , Denote EntityDecl eff (Fix v)
  , Lit Uuid <: v, Lit Address <: v, EntityDecl <: v
  , LitStr <: v, Null <: v
  , Random String String <: eff
  , Show (v (Fix v))
  ) => VarList (FreeEnv eff (Fix v)) ->  FreeEnv eff (Fix v)
denote (Weaken (VList list e)) env = do
  env' <- denoteWeaken list env
  e env'

denoteWeaken :: forall f g v.
  ( Denote EntityDecl f (Fix v)
  , EHeap v <: f,  MLState Address (Fix v) <: f
  , TempEHeap v <: f,  Random String String <: f
  , DbRead (EntityDecl (Fix v)) <: f
  ,  DbWrite (EntityDecl (Fix v)) <: f
  , Lit Uuid <: v,  Lit Address <: v
  , Cond <: f, EntityDecl <: v, Null <: v,  Show (v (Fix v))
  ) => [GlobalVar (FreeEnv f (Fix v))]
    -> Env f (Fix v) -> Free f (Env f (Fix v))
denoteWeaken list env = denoteWeaken' list env id
  -- do 
  -- isSuccess <- connect (Nothing :: MaybeEntity v)
  -- cond' isSuccess
  --   (loadVariables env)
  --   (evaluateVariables list env)

loadVariables :: forall eff eff' v.
  ( Functor eff, EHeap v <: eff',  MLState Address (Fix v) <: eff'
  ,  DbRead (EntityDecl (Fix v)) <: eff'
  ,  Lit Uuid <: v
  ) => Env eff (Fix v)
  -> Free eff' (Env eff (Fix v))
loadVariables env = do
  vars <- loadVars (Nothing :: MaybeEntity v)
  foldM (\env' (name, id) -> do
    (entity :: EntityDecl (Fix v))  <- getEntity id
    assign (id, entity)
    (loc :: Address)                <- ref (box id :: Fix v)
    refGlobalEnv name loc env' )
    env vars

evaluateVariables ::
  (Functor eff', TempEHeap v <: eff,  Heap v <: eff
  , Lit Address <: v, Lit Uuid <: v, EntityDecl <: v
  , Denote EntityDecl eff (Fix v)
  , LitStr <: v, Null <: v
  , Random String String <: eff
  , Show (v (Fix v))
  , TempEHeap v <: eff', Heap v <: eff', EHeap v <: eff'
  , DbWrite (EntityDecl (Fix v)) <: eff'
  ) => [GlobalVar (FreeEnv eff (Fix v))] -> Env eff (Fix v)
    -> (Free eff (Fix v) -> Free eff' (Fix v))
    -> Free eff' (Env eff (Fix v))
evaluateVariables list env lift = do
  env' <- foldM phase1 env (getNames list)
  mapM_ (phase2 env' lift ) list
  mapM_ (phase3 env') (getNames list)
  mapM_ (writeVars env') list
  return env'

-- I should be able to get away with stuffing the environment in as an effect


phase1 :: forall eff eff' v.
  ( Functor eff, TempEHeap v <: eff', Heap v <: eff'
  , Lit Address <: v
  ) => Env eff (Fix v) -> VName
    -> Free eff' (Env eff (Fix v))
phase1 env name = do
  (loc  :: Address)   <- ref (Nothing :: MaybeEntity v)
  (loc' :: Address)   <- ref (box loc :: Fix v)
  refGlobalEnv name loc' env

phase2 :: forall eff eff' v.
  ( Functor eff', Heap v <: eff', TempEHeap v <: eff'
  , Denote EntityDecl eff (Fix v)
  , Lit Address <: v, EntityDecl <: v, LitStr <: v, Null <: v
  , Random String String <: eff
  , Show (v (Fix v))
  , Heap v <: eff, TempEHeap v <: eff
  ) => Env eff (Fix v) -> (Free eff (Fix v) -> Free eff' (Fix v))
    -> GlobalVar (FreeEnv eff (Fix v))
    -> Free eff' ()
phase2 env lift (VDef name entity) = do
  loc              <- derefEnv' name env
  (box' :: Fix v)  <- deref loc
  entity'          <- lift $ denoteEDecl' entity (env :: Env eff (Fix v)) -- doesn't write to environment, only reads. Important: should evaluate objects to fix v and save as such!!
  assign
    ( unbox box' :: Address
    , Just $ projEntity entity')

-- only now we have a guarantee that all objects have SOME uuid
phase3 :: forall eff eff' v.
  ( Functor eff, Heap v <: eff', TempEHeap v <: eff', EHeap v <: eff'
  , DbWrite (EntityDecl (Fix v)) <: eff'
  , Lit Address <: v, Lit Uuid <: v
  ) => Env eff (Fix v) -> VName -> Free eff' ()
phase3 env name = do
  loc                       <- derefEnv' name env
  (box' :: Fix v)           <- deref loc
  (entity :: MaybeEntity v) <- deref (unbox box' :: Address)
  (uuid :: Uuid)            <- updateEntity entity
  assign
    ( loc
    , box uuid :: Fix v )

updateEntity :: forall b f v.
  ( MLState b (EntityDecl (Fix v)) <: f
  , MLState Address (MaybeEntity v) <: f
  , Lit Uuid <: v,  Lit Address <: v
  , DbWrite (EntityDecl (Fix v)) <: f
  ) => Maybe (EntityDecl (Fix v)) -> Free f b
updateEntity (Just (EDecl name params)) = do
  params'    <- mapM mapParams params
  setEntity (EDecl name params')
  ref       (EDecl name params') -- here mapping

mapParams :: forall f v a.
  (MLState Address (MaybeEntity v) <: f
  , Lit Uuid <: v,  Lit Uuid <: v, Lit Address <: v
  ) => (a, Fix v) -> Free f (a, Fix v)
mapParams (name, value) = case projF value of
  (Just (Box (address :: Address))) -> do
    (entity :: MaybeEntity v) <- deref address
    return
      ( name
      , box $ getUuid $ fromJust entity )
  _                       -> return (name, value)

writeVars :: forall f g v. (Heap v <: g, Functor f
  , DbWrite (EntityDecl (Fix v)) <: g
  , Lit Uuid <: v
  ) => Env f (Fix v) -> GlobalVar (FreeEnv f (Fix v))
  -> Free g ()
writeVars env (VDef name _) = do
  loc            <- derefEnv' name env
  (id :: Fix v)  <- deref loc
  setVar
    ( Nothing :: MaybeEntity v )
      name
    ( unbox id :: Uuid )


-- I think this is all?????
