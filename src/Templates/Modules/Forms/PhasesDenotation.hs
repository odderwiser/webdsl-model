{-# OPTIONS_GHC -Wno-missing-fields #-}
module Templates.Modules.Forms.PhasesDenotation where
import Templates.Effects as E
import Utils
import Actions.Effects ( Random, deref, MLState, assign, ref, EHeap )
import Actions.Bool (LitBool)
import Actions.Arith (LitInt)
import Actions.Str (LitStr)
import Templates.Modules.Forms.Syntax
import Actions.Values
import Syntax (Type(..), Address)
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Modules.Eval.Syntax (Eval (Var))
import Actions.Modules.Eval.Denotation (derefEnv)
import Actions.Modules.Entity.Syntax (Entity (PropAccess))
import Definitions.GlobalVars.Syntax (Uuid)
import Text.Read (readMaybe)
import Actions.Modules.Entity.Denotation (setProperty)


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

denoteDb :: forall eff eff' eff'' v v' g.
  ( State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff',  ReqParamsSt <: eff'
  , MLState TVarAddress v <: eff'
  , Heap v' <: eff', EHeap v' <: eff', Throw <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lit TVarAddress <: v', PropRef <: v'
  , Lift eff'' eff' v, Denote g eff'' (Fix v'), eff''~Heap v' + End)
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDb (Input exp Bool) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: Maybe LabelId<- get
  seed         :: Seed         <- get
  inputName    :: String       <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- deref $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef <- lift (Utils.foldD exp $ liftEnv (actionEnv env) :: Free eff'' v)
      boundParam :: Maybe String <- deref inputName
      case boundParam of
        Just "true"  -> bindValue valueRef (injF $ V True :: v)
        Just "false" -> bindValue valueRef (injF $ V False :: v)
        _ -> throw "not a well-formed boolean value"
    Nothing -> return ()

denoteDb (Input exp String) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: Maybe LabelId<- get
  seed         :: Seed         <- get
  inputName    ::String        <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- deref $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef <- lift (Utils.foldD exp $ liftEnv (actionEnv env) :: Free eff'' v)
      boundParam :: Maybe String <- deref inputName
      case boundParam of
        Just str  -> bindValue valueRef (injF $ V str :: v) -- look out for scripts? idk 
        _ -> throw "not a well-formed String value"
    Nothing -> return ()

denoteDb (Input exp Int) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: LabelId      <- get
  seed         :: Seed         <- get
  inputName    :: String       <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- deref $ "form_"++formId
  case isActiveForm of
    (Just "1") -> do
      valueRef <- lift (Utils.foldD exp $ liftEnv (actionEnv env) :: Free eff'' v)
      boundParam :: Maybe String <- deref inputName
      case boundParam of
        Just int  -> case readMaybe int of
          Just (int' :: Int) -> bindValue valueRef (injF $ V int :: v) -- look out for scripts? idk 
          Nothing -> throw "not a well-formed Int value"
        _ -> throw "not a well-formed String value"
    Nothing -> return ()

denoteV :: (State FormId <: eff', State (Maybe LabelId) <: eff'
  , State Seed <: eff', Random Label LabelId <: eff'
  , ReqParamsSt <: eff')
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteV (Input exp _) env = do
  formId       :: String       <- get
  label        :: Maybe LabelId<- get
  seed         :: Seed         <- get
  inputName    :: String       <- encode $ show label ++ show seed
  isActiveForm :: Maybe String <- deref $ "form_"++formId
  -- here some validation should happen: possibly if exp is an entity
  -- it should have some validation tuples and these can be rechecked?
  return ()

-- so far this only allows in-place, anonymous action definitions
denoteAction :: (Lift eff eff' v
  ,  State FormId <: eff', State ButtonCount <: eff'
  ,  ReqParamsSt <: eff', Lit String <: v', v~Fix v'
  ,  Random Label LabelId <: eff', State (Maybe LabelId) <: eff'
  ,  State Seed <: eff', LitStr <: v')
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteAction (Submit action name) env = do
  name <- lift $ name $ actionEnv env
  formId :: String <- get
  buttonCount :: ButtonCount <- get
  isButtonPressed :: Maybe String <- deref $ "withForms_ia" ++ show buttonCount ++ "_" ++ formId
  case isButtonPressed  of
    Just name' -> case name' == unbox name of
      True -> lift $ action $ actionEnv env
  return ()

denoteAction other env = denoteProcess other env


bindValue :: (MLState TVarAddress v <: f,
  Heap v' <: f, EHeap v' <: f,
  Lit TVarAddress <: v', PropRef <: v', v~Fix v' )
  => v -> v -> Free f ()
bindValue valueRef value = case projF valueRef of
  Just (Box a@(Address address)) -> assign (a, value)
  Nothing -> case projF valueRef of
    Just (PropRef (uuid, name))  -> do
      entity <- deref uuid
      entity' <- setProperty name value entity
      uuid :: Uuid <- ref entity'
      return ()

liftEnv :: Env eff v -> Env eff'' v
liftEnv env = Env {varEnv=varEnv env, globalVars= globalVars env }

--this is a minimum implementation of the alternative denotation
denoteRefEval :: (Heap v' <: eff, v~ Fix v')
  => Eval (FreeEnv eff v) -> FreeEnv eff v
denoteRefEval (Var name) env = do
  loc <- derefEnv name env
  deref loc

denoteRefEntity :: (Heap v' <: eff, v~ Fix v'
  , PropRef <: v', Lit Uuid <: v')
  => Entity (FreeEnv eff v) -> FreeEnv eff v
denoteRefEntity (PropAccess obj pName) env = do
  uuid <- obj env
  return $ injF $ PropRef (unbox uuid, pName)