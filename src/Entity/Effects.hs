module Entity.Effects where
import Utils.Composition
import Utils.Free
import Actions.Effects
import Entity.Syntax (EntityDef)
import Syntax (Type)

data Write e k = Write e k
  deriving Functor

write :: Write e <: f => e -> Free f ()
write value = Op $ inj $ Write value $ Pure ()

data MutateEnv env k =
  Drop (DropEnv env k)
  | LiftObjectEnv env env (env -> k)
  | GenerateEmptyEnv   (env -> k) 
  | PopulateObjEnv env env (env -> k)  
  deriving Functor

lift :: forall f env eff free. 
  (eff ~ MutateEnv env, free ~ Free f env, 
  eff <: f) => env -> env -> free
lift globalEnv objEnv = Op $ inj 
  $ LiftObjectEnv globalEnv objEnv Pure

genEnv :: forall f env eff free. 
  (eff ~ MutateEnv env, free ~ Free f env, 
  eff <: f) => free
genEnv = Op $ inj 
  $ GenerateEmptyEnv Pure


populateMissingDefault objEnv defaultObjEnv = Op $ inj
  $ PopulateObjEnv objEnv defaultObjEnv Pure

data DefaultValue e k = 
  DefaultValue Type (e -> k)
  deriving Functor

defaultType ty = Op $ inj $ DefaultValue ty Pure