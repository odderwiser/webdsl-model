module Actions.Effects where

import Utils.Composition
import Utils.Free
import Utils.Fix
import Actions.Modules.Bool.Syntax (LitBool, projBool)
import Syntax (Type, Address)
import Utils.Environment
import Actions.Modules.Entity.Syntax
import Definitions.GlobalVars.Syntax (Uuid)

--- ABORT ---

data Abort v k = Abort v
  deriving (Functor, Show)

abort :: Abort v <: f => v ->  Free f v
abort val = Op (inj (Abort val))

--- COND ---

data Cond k =  Cond Bool k k
  deriving (Functor, Show)

cond :: (fix ~ Fix a, Cond <: f, LitBool <: a) 
  => fix -> Free f fix -> Free f fix 
  -> Free f fix
cond bool k1 k2 = Op . inj
    $ Cond (projBool bool) k1 k2

cond' :: (Cond <: f) 
  => Bool -> Free f fix -> Free f fix 
  -> Free f fix
cond' bool k1 k2 = Op . inj
    $ Cond bool k1 k2

--- MLState ---

data MLState m v k
  = Ref v (m -> k)
  | Deref m (v -> k)
  | Assign (m, v) k
  deriving Functor

instance Show (MLState m v k) where 
  show :: MLState m v k -> String
  show ant = "dummy show"

deref :: MLState m v <: f => m -> Free f v
deref key = Op $ inj $ Deref key Pure

assign :: MLState m v <: f => (m, v) -> Free f ()
assign pair = Op $ inj $ Assign pair $ Pure ()

ref :: MLState m v <: f => v -> Free f m
ref val = Op $ inj $ Ref val Pure

--- DropEnv ---

data DropEnv env k 
  = DropLocalVars env (env ->  k)
  | DropAttributes env (env -> k)
  --objects only
  deriving Functor

drop :: DropEnv env <: f => env -> Free f env
drop env = Op $ inj $ DropLocalVars env Pure

--- Entity stuff --

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

data DefaultValue e k = 
  DefaultValue Type (e -> k)
  deriving Functor

defaultType ty = Op $ inj $ DefaultValue ty Pure

populateMissingDefault objEnv defaultObjEnv = Op $ inj
  $ PopulateObjEnv objEnv defaultObjEnv Pure

data Random e v k = Random e (v -> k)
  deriving Functor

random :: (Show e, Random String v <: f) => e -> Free f v
random seed = Op $ inj $ Random (show seed) Pure

type DbKey = Uuid
type EHeap v = MLState DbKey (EntityDecl (Fix v))