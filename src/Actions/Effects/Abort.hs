module Actions.Effects.Abort where

import Utils.Composition
import Utils.Free

data Abort v k = Abort v
  deriving Functor

abort :: Abort v <: f => v -> Free f a
abort val = Op (inj (Abort val))