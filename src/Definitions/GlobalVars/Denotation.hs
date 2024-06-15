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
import Actions.Modules.Eval.Denotation (refEnv, derefEnv)
import Data.Maybe (fromJust)
import Actions.Values
import Actions.Handlers.Entity hiding (updateEntity)
import Templates.Modules.Lift.Syntax

-- denoteDef :: forall eff v. (MLState Address (Fix v) <: eff,
--     EntityDecl <: v, LitAddress <: v, Random String String <: eff,
--     LitStr <: v, Null <: v)
--     =>  GlobalVar (FreeEnv eff (Fix v)) -> Env eff (Fix v) -> Free eff ()
-- denoteDef (VDef name eDecl) env = do
--     obj :: Fix v <- denoteEDecl eDecl env

--     return ()

type VarEnv = MLState VName Address
type Heap v = MLState Address (Fix v)

denote :: forall eff v.
  ( DbRead (EntityDecl (Fix v)) <: eff, Cond <: eff, EHeap v <: eff
  , Heap v <: eff, TempEHeap v <: eff, DbWrite (EntityDecl (Fix v)) <: eff
  , Denote EntityDecl eff (Fix v)
  , Lit Uuid <: v, Lit Address <: v, EntityDecl <: v
  , LitStr <: v, Null <: v
  , Random String String <: eff
  , Show (v (Fix v))
  ) => VarList (FreeEnv eff (Fix v)) -> FreeEnv eff (Fix v)
denote (Weaken (VList list e)) env = do
  isSuccess <- connect (Nothing :: MaybeEntity v)
  env' <- cond' isSuccess
    (loadVariables env)
    (evaluateVariables list env)
  e env'

loadVariables :: forall eff v.
  ( EHeap v <: eff,  MLState Address (Fix v) <: eff
  ,  DbRead (EntityDecl (Fix v)) <: eff
  ,  Lit Uuid <: v
  ) => Env eff (Fix v) -> Free eff (Env eff (Fix v))
loadVariables env = do
  vars <- loadVars (Nothing :: MaybeEntity v)
  foldM (\env' (name, id) -> do
    (entity :: EntityDecl (Fix v))  <- getEntity id
    assign (id, entity)
    (loc :: Address)                <- ref (box id :: Fix v)
    refEnv name loc env' )
    env vars

evaluateVariables ::
  (TempEHeap v <: eff,  MLState Address (Fix v) <: eff, EHeap v <: eff
  , DbWrite (EntityDecl (Fix v)) <: eff
  , Lit Address <: v, Lit Uuid <: v, EntityDecl <: v
  , Denote EntityDecl eff (Fix v)
  , LitStr <: v, Null <: v
  , Random String String <: eff
  , Show (v (Fix v))
  ) => [GlobalVar (FreeEnv eff (Fix v))] -> Env eff (Fix v)
    -> Free eff (Env eff (Fix v))
evaluateVariables list env = do
  env' <- foldM phase1 env (getNames list)
  mapM_ (phase2 env') list
  mapM_ (phase3 env') (getNames list)
  mapM_ (writeVars env') list
  return env'

-- I should be able to get away with stuffing the environment in as an effect


phase1 :: forall eff v.
  ( TempEHeap v <: eff, Heap v <: eff
  , Lit Address <: v
  ) => Env eff (Fix v) -> VName
    -> Free eff (Env eff (Fix v))
phase1 env name = do
  (loc  :: Address)   <- ref (Nothing :: MaybeEntity v)
  (loc' :: Address)   <- ref (box loc :: Fix v)
  refEnv name loc' env

phase2 :: forall eff v.
  ( Heap v <: eff, TempEHeap v <: eff
  , Denote EntityDecl eff (Fix v)
  , Lit Address <: v, EntityDecl <: v, LitStr <: v, Null <: v
  , Random String String <: eff
  , Show (v (Fix v))
  ) => Env eff (Fix v) -> GlobalVar (FreeEnv eff (Fix v))
    -> Free eff ()
phase2 env (VDef name entity) = do
  loc              <- derefEnv name env
  (box' :: Fix v)  <- deref loc
  entity'          <- denoteEDecl' entity (env :: Env eff (Fix v)) -- doesn't write to environment, only reads. Important: should evaluate objects to fix v and save as such!!
  assign
    ( unbox box' :: Address
    , Just $ entity')

-- only now we have a guarantee that all objects have SOME uuid
phase3 :: forall eff v.
  ( Heap v <: eff, TempEHeap v <: eff, EHeap v <: eff
  , DbWrite (EntityDecl (Fix v)) <: eff
  , Lit Address <: v, Lit Uuid <: v
  ) => Env eff (Fix v) -> VName -> Free eff ()
phase3 env name = do
  loc                       <- derefEnv name env
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

writeVars :: forall f v. (Heap v <: f
  , DbWrite (EntityDecl (Fix v)) <: f
  , Lit Uuid <: v
  ) => Env f (Fix v) -> GlobalVar (FreeEnv f (Fix v))
  -> Free f ()
writeVars env (VDef name _) = do
  loc            <- derefEnv name env
  (id :: Fix v)  <- deref loc
  setVar 
    ( Nothing :: MaybeEntity v ) 
      name 
    ( unbox id :: Uuid )


-- I think this is all?????
