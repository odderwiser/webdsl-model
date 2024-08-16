module Templates.Modules.Attributes.Denotation where
import Templates.Modules.Attributes.Syntax
import Utils
import Templates.Effects
import Actions.Handlers.Env (derefH)
import Data.List (delete)
import Data.Maybe (fromJust)
import Templates.Handlers.Env (attsH)

denote :: forall eff eff' v. (
    Lift eff eff' v,
    State [(AttName, String)] <: eff', Render v <: eff')
  => Attributes (PEnv eff eff' v) (FreeEnv eff v)
    -> PEnv eff eff' v
denote (SelectionList list body) env = do
    mapM_ (\el -> denoteSel el env) list
    body env

denoteSel :: (  
    Lift eff eff' v,
    State [(AttName, String)] <: eff', Render v <: eff') => 
    AttributeSel (PEnv eff eff' v) (FreeEnv eff v)
    -> PEnv eff eff' v
denoteSel (Attribute attName) env = do
    (v :: String) <- derefH attName attsH env
    put [(attName, v)]

denoteSel (AllAttributes list) env = do
    mapM_ put $ getAllExcept (attributes env) list

denoteSel (AttDef name e) env = do
    e' <- lift $ e $ actionEnv env
    e'' <- render e' -- this probably doesn't do what I want it to 
    put [(name, e'')] 

getAllExcept env = map ((`delete` env) 
    . (\elem -> (elem, fromJust $ lookup elem env )))