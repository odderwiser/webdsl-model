module Definitions.GlobalVars.Denotation where
import Actions.Effects
import Syntax as S
import Actions.Syntax
import Definitions.GlobalVars.Syntax
import Utils.Environment (FreeEnv)
import Utils  as U
import Actions.Modules.Entity.Denotation (denoteEDecl)
import Actions.Modules.Str.Syntax (LitStr)
import Data.Traversable
import Control.Monad (foldM)
import Definitions.Entity.Denotation
import Definitions.GlobalVars.Effects (connect, loadVars, getEntity, DbRead, setVar, setEntity, DbWrite)
import Actions.Modules.Eval.Denotation (refEnv, derefEnv)
import Data.Maybe (fromJust)

-- denoteDef :: forall eff v. (MLState Address (Fix v) <: eff,
--     EntityDecl <: v, LitAddress <: v, Random String String <: eff,
--     LitStr <: v, Null <: v)
--     =>  GlobalVar (FreeEnv eff (Fix v)) -> Env eff (Fix v) -> Free eff ()
-- denoteDef (VDef name eDecl) env = do
--     obj :: Fix v <- denoteEDecl eDecl env

--     return ()

type VarEnv = MLState VName Address
type MaybeEntity v = Maybe (EntityDecl (Fix v))
type TempEHeap v = MLState Address (MaybeEntity v)
type Heap v = MLState Address (Fix v)

denote :: forall eff v.
  ( DbRead (EntityDecl (Fix v)) <: eff, Cond <: eff, EHeap v <: eff
  , Heap v <: eff, TempEHeap v <: eff, DbWrite (EntityDecl (Fix v)) <: eff
  , Denote EntityDecl eff (Fix v)
  , LitV Uuid <: v, LitV Address <: v, EntityDecl <: v
  ) => VarList (FreeEnv eff (Fix v)) -> FreeEnv eff (Fix v)
denote (VList dbEntry list e) env = do
  isSuccess <- connect (Nothing :: MaybeEntity v)
  env' <- cond' isSuccess
    (loadVariables env)
    (evaluateVariables list env)
  e env'

loadVariables :: forall eff v.
  ( EHeap v <: eff,  MLState Address (Fix v) <: eff
  ,  DbRead (EntityDecl (Fix v)) <: eff
  ,  LitV Uuid <: v
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
  , LitV Address <: v, LitV Uuid <: v, EntityDecl <: v
  , Denote EntityDecl eff (Fix v)
  ) => [GlobalVar (FreeEnv eff (Fix v))] -> Env eff (Fix v)
    -> Free eff (Env eff (Fix v))
evaluateVariables list env = do
  env' <- foldM phase1 env (getNames list)
  mapM_ (phase2 env) list
  mapM_ (phase3 env) (getNames list)
  mapM_ (writeVars env) list
  return env

-- I should be able to get away with stuffing the environment in as an effect


phase1 :: forall eff v.
  ( TempEHeap v <: eff, Heap v <: eff
  , LitV Address <: v
  ) => Env eff (Fix v) -> VName
    -> Free eff (Env eff (Fix v))
phase1 env name = do
  (loc  :: Address)   <- ref (Nothing :: MaybeEntity v)
  (loc' :: Address)   <- ref (box loc :: Fix v)
  refEnv name loc env

phase2 :: forall eff v.
  ( Heap v <: eff, TempEHeap v <: eff
  , Denote EntityDecl eff (Fix v)
  , LitV Address <: v, EntityDecl <: v
  ) => Env eff (Fix v) -> GlobalVar (FreeEnv eff (Fix v))
    -> Free eff ()
phase2 env (VDef name entity) = do
  loc              <- derefEnv name env
  (box' :: Fix v)  <- deref loc
  entity'          <- U.denote entity env -- doesn't write to environment, only reads. Important: should evaluate objects to fix v and save as such!!
  assign
    ( fromJust $ unbox box' :: Address
    , Just $ projEntity entity')

-- only now we have a guarantee that all objects have SOME uuid
phase3 :: forall eff v.
  ( Heap v <: eff, TempEHeap v <: eff, EHeap v <: eff
  , DbWrite (EntityDecl (Fix v)) <: eff
  , LitV Address <: v, LitV Uuid <: v
  ) => Env eff (Fix v) -> VName -> Free eff ()
phase3 env name = do
  loc              <- derefEnv name env
  (box' :: Fix v)  <- deref loc
  (entity :: MaybeEntity v) <- deref (fromJust $ unbox box' :: Address)
  (uuid :: Uuid) <- case entity of
    Just entity'@(EDecl name ps) -> do
      params <- mapM (\(name, value) -> case projF value of
        Nothing -> return (name, value)
        (Just (Box (address :: Address))) -> do
          (entity' :: MaybeEntity v) <- deref address
          return (name, box $ fromJust $ getUuid $ fromJust entity)
        ) ps
      setEntity (EDecl name params)
      ref (EDecl name params) -- here mapping
  assign (loc, box uuid :: Fix v)
  return ()


writeVars :: forall f v. (Heap v <: f
  , DbWrite (EntityDecl (Fix v)) <: f
  , LitV Uuid <: v
  ) => Env f (Fix v) -> GlobalVar (FreeEnv f (Fix v))
  -> Free f ()
writeVars env (VDef name _) = do
  loc <- derefEnv name env
  (id :: Fix v)  <- deref loc
  setVar (Nothing :: MaybeEntity v) name (fromJust $ unbox id :: Uuid)


-- I think this is all?????
