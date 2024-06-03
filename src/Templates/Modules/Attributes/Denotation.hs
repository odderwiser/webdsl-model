module Templates.Modules.Attributes.Denotation where
import Templates.Modules.Attributes.Syntax
import Utils
import Templates.Effects
import Actions.Handlers.Env (derefH)
import Data.List (delete)
import Data.Maybe (fromJust)
import Templates.Handlers.Env (attsH)
denote :: forall eff v. (
    State (AttList (Fix v)) <: eff)
  => Attributes (Env eff (Fix v) -> Free eff (Fix v))
  -> Env eff (Fix v) -> Free eff ()
denote (SelectionList list) env = do
    mapM_ (\el -> el env) list

denoteSel (Attribute attName) env = do
    (v :: String) <- derefH attName attsH env
    put (attName, v)

denoteSel (AllAttributes list) env = do
    mapM_ put $ getAllExcept (attributes env) list

denoteSel (AttDef name e) env = do
    e' <- e env
    e'' <- render e'
    put (name, e'')

getAllExcept env = map ((`delete` env) 
    . (\elem -> (elem, fromJust $ lookup elem env )))