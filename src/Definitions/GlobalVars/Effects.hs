module Definitions.GlobalVars.Effects where
import Definitions.Entity.Syntax (EName, PName)
import Actions.Modules.Eval.Syntax (VName)
import Utils
import Definitions.GlobalVars.Syntax (Uuid)
import Actions.Modules.Entity.Syntax (EntityDecl)

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

data DbWrite e v k
    = SetEntity e k
    | SetVar (VName, Uuid) k 
    | UpdateEntity Uuid PName v k
    deriving Functor

type DbWrite' v = DbWrite (EntityDecl (Fix v)) (Fix v)

setEntity :: forall e v f. (DbWrite e v <: f) => e -> Maybe v -> Free f ()
setEntity entity _ = Op $ inj (SetEntity entity $ Pure () :: DbWrite e v (Free f ()))

setVar :: forall e v f. (DbWrite e v <: f) => Maybe (e, v) -> VName -> Uuid -> Free f ()
setVar _ name value = Op $ inj (SetVar (name, value) $ Pure () :: DbWrite e v (Free f ()))

updateProp :: forall e v f. (DbWrite e v <: f) => Uuid -> PName -> v -> Maybe e -> Free f ()
updateProp id prop v _  = Op $ inj (UpdateEntity id prop v $ Pure () :: DbWrite e v (Free f ()))