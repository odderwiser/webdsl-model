module Templates.Modules.Phases.Denotation where
import Templates.Modules.Phases.Syntax
import Utils
import Actions.Values (unbox, Lit (V))
import Templates.Effects (Throw, throw)
import Actions.Bool (LitBool)

denoteDb :: (Functor eff, Functor eff')
  => Databind (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDb (Databind exp) env = return ()

denoteDbDatabind :: (Lift eff eff' v)
  => Databind (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDbDatabind (Databind exp) env = do
  exp' <- lift $ exp (actionEnv env)
  return ()

denoteV :: (Functor eff, Functor eff')
  => Validate (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteV v env = return ()

denoteVValidate :: (Lift eff eff' v, Throw <: eff'
  , v ~ Fix v', LitBool <: v')
  => Validate (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteVValidate (Validate e error) env = do
  value <- lift $ e (actionEnv env)
  if unbox value 
    then return () 
    else throw error

denoteA :: (Functor eff, Functor eff')
  => Action (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteA action env = return ()

denoteAAction :: (Lift eff eff' v)
  => Action (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteAAction (Action a) env = do
  value <- lift $ a (actionEnv env)
  return ()