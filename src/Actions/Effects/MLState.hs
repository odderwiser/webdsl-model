module Actions.Effects.MLState where

import Utils.Composition
import Utils.Free (Free(..))

data MLState m v k
  = Ref v (m -> k)
  | Deref m (v -> k)
  | Assign (m, v) k
  deriving Functor

deref :: MLState m v <: f => m -> Free f v
deref key = Op $ inj $ Deref key Pure

assign :: MLState m v <: f => (m, v) -> Free f ()
assign pair = Op $ inj $ Assign pair $ Pure ()

ref :: MLState m v <: f => v -> Free f m
ref val = Op $ inj $ Ref val Pure
