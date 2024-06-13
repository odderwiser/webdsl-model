module Definitions.GlobalVars.Effects where
import Definitions.Entity.Syntax (EName)
import Actions.Modules.Eval.Syntax (VName)
import Utils
import Definitions.GlobalVars.Syntax (Uuid)

type IsSuccess = Bool

data DbRead e k
    = GetEntity Uuid (e -> k)
    | GetAll EName ([e] -> k)
    | Connect (IsSuccess -> k)
    | LoadVariables ([(VName, Uuid)] -> k)
    deriving Functor

getEntity :: (DbRead e <: f) => Uuid -> Free f e
getEntity uuid = Op $ inj $ GetEntity uuid Pure

getAll :: (DbRead e <: f) => EName -> Free f [e]
getAll eName = Op $ inj $ GetAll eName Pure

connect :: forall e f. (DbRead e <: f) => Maybe e -> Free f IsSuccess -- this might not work?
connect _ = Op $ inj (Connect Pure :: DbRead e (Free f IsSuccess)) 

loadVars :: forall e f. (DbRead e <: f) => Maybe e -> Free f [(VName, Uuid)]
loadVars _ = Op $ inj (LoadVariables Pure :: DbRead e (Free f [(VName, Uuid)]))

data DbWrite e k
    = SetEntity e k
    | SetVar (VName, Uuid) k 
    deriving Functor

setEntity :: (DbWrite e <: f) => e -> Free f ()
setEntity entity = Op $ inj $ SetEntity entity $ Pure ()

setVar :: forall e f. (DbWrite e <: f) => Maybe e -> VName -> Uuid -> Free f ()
setVar _ name value = Op $ inj (SetVar (name, value) $ Pure () :: DbWrite e (Free f ()))