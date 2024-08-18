{-# OPTIONS_GHC -Wno-missing-fields #-}
module Templates.Modules.Forms.PhasesDenotation where
import Templates.Effects as E
import Utils
import Actions.Effects ( Random, deref, MLState, assign, ref, EHeap, Writer, write )
import Actions.Bool (LitBool)
import Actions.Arith (LitInt)
import Actions.Str (LitStr)
import Templates.Modules.Forms.Syntax
import Actions.Values
import Syntax (Type(..), Address)
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Modules.Eval.Syntax (Eval (Var))
import Actions.Modules.Eval.Denotation (derefEnv)
import Actions.Modules.Entity.Syntax (Entity (PropAccess), projEntity, EntityDecl (..), projEName, projParams)
import Text.Read (readMaybe)
import Actions.Modules.Entity.Denotation (setProperty, getObj', refProperties)
import Definitions.GlobalVars.Effects (DbRead)
import Templates.Modules.Page.Syntax (TId (TId))
import Actions.Modules.Phases.Syntax (VTuple(Validate))
import Definitions.Entity.Syntax
import Actions.Handlers.Env (derefH)
import Actions.Handlers.Entity (entityDefsH)
import Data.Maybe (fromJust)
import Templates.Modules.Lift.Syntax (LiftE(LiftE))
import Actions.Modules.Phases.Denotation as D hiding (denoteRef) 


denoteProcess :: forall eff eff' v v'.
  ( State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff'
  ,LitStr <: v', v ~ Fix v'
  , Lift eff eff' v)
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteProcess (Form _ body) env = do
  seed :: Seed     <- get
  formId :: String <- encode $ "form_" ++ show seed
  put formId
  body env

denoteProcess (Label name contents) env = do -- attribute "for"
  name'            <- lift $ name $ actionEnv env
  nameId :: String <- encode (unbox name' :: Label)
  put (Just nameId)
  contents env

denoteProcess (Submit _ _) env = return ()

denoteDb :: forall eff eff' v v' g.
  ( State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff',  ReqParamsSt <: eff'
  , MLState TVarAddress v <: eff'
  , Heap v' <: eff', EHeap v' <: eff', Writer (TId, String) <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lit TVarAddress <: v', PropRef <: v', DbRead (EntityDecl (Fix v')) <: eff', EntityDecl <: v'
  , Lift eff eff' v, Heap v' <: eff, Eval <: g, Entity <: g, Denote g eff v
  , Show (v' (Fix v')))
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDb (Input exp Bool) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: Maybe LabelId<- get
  seed         :: Seed         <- get
  inputName    :: String       <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- E.read $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef                   <- lift $ denoteRef exp $ actionEnv env
      boundParam :: Maybe String <- E.read inputName
      case boundParam of
        Just "true"  -> bindValue valueRef (injF $ V True :: v)
        Just "false" -> bindValue valueRef (injF $ V False :: v)
        _ ->  write (TId (templateId $ actionEnv env), "not a well-formed boolean value")
    Nothing -> return ()

denoteDb (Input exp String) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: Maybe LabelId<- get
  seed         :: Seed         <- get
  inputName    ::String        <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- E.read  $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef <- lift $ denoteRef exp $ actionEnv env
      boundParam :: Maybe String <- E.read inputName
      case boundParam of
        Just str  -> bindValue valueRef (injF $ V str :: v) -- look out for scripts? idk 
        _ ->  write (TId (templateId $ actionEnv env), "not a well-formed String value")
    Nothing -> return ()

denoteDb (Input exp Int) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: Maybe LabelId <- get
  seed         :: Seed         <- get
  inputName    :: String       <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- E.read  $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef <- lift $ denoteRef exp $ actionEnv env
      boundParam :: Maybe String <- E.read inputName
      case boundParam of
        Just int  -> case readMaybe int of
          Just (int' :: Int) -> bindValue valueRef (injF $ V int' :: v) -- look out for scripts? idk 
          Nothing ->  write (TId (templateId $ actionEnv env), "not a well-formed Int value")
        _ ->  write (TId (templateId $ actionEnv env),"value missing ")
    Nothing -> return ()

denoteRef :: forall g v' eff.
  (Eval <: g, Entity <: g, Heap v' <: eff, Lit Uuid <: v', PropRef <: v', Denote g eff (Fix v'))
  => Fix g -> Env eff (Fix v') -> Free eff (Fix v')
denoteRef (In syntax) = case proj syntax of
  (Just (e :: Eval (Fix g))) -> denoteRefEval e
  Nothing                    -> case proj syntax of
    (Just (e :: Entity (Fix g))) -> denoteRefEntity e

denoteV :: forall eff eff' g v v'.(State FormId <: eff', State (Maybe LabelId) <: eff'
  , State Seed <: eff', Random Label LabelId <: eff', Heap v' <: eff, Heap v' <: eff'
  , PropRef <: v', v~ Fix v', Lit Uuid <: v', LitBool <: v', Writer (TId, String) <: eff'
  , ReqParamsSt <: eff', Lift eff eff' (Fix v'), EntityDecl <: v'
  , Eval <: g, Entity <: g, Denote g eff v, EHeap v' <: eff', DbRead (EntityDecl (Fix v')) <: eff'
  , Show (v' (Fix v')))
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteV (Input exp _) env = do
  formId       :: String       <- get
  label        :: Maybe LabelId<- get
  seed         :: Seed         <- get
  inputName    :: String       <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- E.read  $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef <- lift $ denoteRef exp $ actionEnv env
      case fromJust $ projF valueRef of
        PropRef (entity, propName) -> do
          entity' :: v <-  getObj' entity
          let (EDecl eName params) = projEntity entity'
          (EDef name _ _ _ validation) <- derefH eName entityDefsH $ actionEnv env
          locs :: [Address]     <- mapM (ref . snd) params
          env'                 <- refProperties (map fst params) locs $ actionEnv env
          mapM_ (\e -> D.denoteT (LiftE e) env {actionEnv = env'}) 
            (filter (\(Validate _ _ list) ->  elem propName list) validation)
    Nothing -> return ()
  return ()



denoteA :: (Functor eff, Functor eff')
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteA (Input exp _) env =
  return ()


-- so far this only allows in-place, anonymous action definitions
denoteAction :: forall eff eff' v v'. (Lift eff eff' v
  ,  State FormId <: eff', State ButtonCount <: eff'
  ,  ReqParamsSt <: eff', Lit String <: v', v~Fix v'
  ,  Random Label LabelId <: eff', State (Maybe LabelId) <: eff'
  ,  State Seed <: eff', LitStr <: v')
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteAction (Submit action name) env = do
  name' :: (Fix v') <- lift $ name $ actionEnv env
  formId :: String <- get
  buttonCount :: ButtonCount <- get
  isButtonPressed :: Maybe String <- E.read 
    $ "withForms_ia" ++ show buttonCount ++ "_" ++ formId
  case isButtonPressed  of
    Just name'' -> case name'' == (unbox name' :: String)  of
      True -> action  env
    Nothing -> return ()
  return ()

denoteAction other env = denoteProcess other env

bindValue :: (MLState TVarAddress v <: f,
  Heap v' <: f, EHeap v' <: f, DbRead (EntityDecl v) <: f,
  Lit TVarAddress <: v', PropRef <: v', EntityDecl <: v', v~Fix v', Show (v' (Fix v')) )
  => v -> v -> Free f ()
bindValue valueRef value = do
  case projF valueRef of
    Just (PropRef (uuid, name))  -> do
      entity <- getObj' uuid
      entity' <- setProperty name value $ projEntity entity
      uuid :: Uuid <- ref $ Just entity'
      return ()
    Nothing -> case projF valueRef of
      Just (Box (Address address)) -> assign (Address address, value)

liftEnv :: Env eff v -> Env eff'' v
liftEnv env = Env {varEnv=varEnv env, globalVars= globalVars env }

--this is a minimum implementation of the alternative denotation
denoteRefEval :: (Heap v' <: eff, v~ Fix v')
  => Eval (Fix g) -> FreeEnv eff v
denoteRefEval (Var name) env = do
  loc <- derefEnv name env
  deref loc

denoteRefEntity :: (Heap v' <: eff, v~ Fix v'
  , PropRef <: v', Lit Uuid <: v', Denote g eff (Fix v'))
  => Entity (Fix g) -> FreeEnv eff v
denoteRefEntity (PropAccess obj pName) env = do
  uuid <- Utils.foldD obj env
  return $ injF $ PropRef (unbox uuid, pName)