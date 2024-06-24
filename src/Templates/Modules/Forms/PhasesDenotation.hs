{-# OPTIONS_GHC -Wno-missing-fields #-}
module Templates.Modules.Forms.PhasesDenotation where
import Templates.Effects as E
import Utils
import Actions.Effects ( Random, deref, MLState, assign )
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


denoteDb :: forall eff eff' v v'.
  ( State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff',  ReqParamsSt <: eff'
  , State ButtonCount <: eff', MLState TVarAddress v <: eff'
  , Heap v' <: eff', Throw <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lit TVarAddress <: v'
  , Lift eff eff' v)
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDb (Form _ body) env = do
  seed :: Seed     <- get
  formId :: String <- encode $ "form_" ++ show seed
  put formId
  body env
  reset

denoteDb (Label name contents) env = do -- attribute "for"
  name'            <- lift $ name $ actionEnv env
  nameId :: String <- encode (unbox name' :: Label)
  put (Just nameId)
  contents env

denoteDbI :: forall eff eff' eff'' v v' g.
  ( State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff', State FormId <: eff',  ReqParamsSt <: eff'
  , State ButtonCount <: eff', MLState TVarAddress v <: eff'
  , Heap v' <: eff', Throw <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lit TVarAddress <: v'
  , Lift eff'' eff' v, Denote g eff'' (Fix v'))
  => Input (Fix g) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDbI (Input exp Bool) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: LabelId      <- get
  seed         :: Seed         <- get
  inputName    ::String        <- encode $ show label ++ show seed
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

denoteDbI (Input exp String) env = do --exp is a reference to param or template variable
  formId       :: String       <- get
  label        :: LabelId      <- get
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

denoteDbI (Input exp Int) env = do --exp is a reference to param or template variable
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

bindValue :: (MLState TVarAddress v <: f,
  Heap v' <: f,
  Lit TVarAddress <: v', Lit Int <: v', v~Fix v' )
  => v -> v -> Free f ()
bindValue valueRef value = case projF valueRef of
  Just (Box a@(Address address)) -> assign (a, value)
  Nothing -> case projF valueRef of
    Just (Box (address :: Int))  -> assign (address, value)

-- denoteRe
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