module Attributes.Effects where
import Utils.Fix
import Utils.Free
import Utils.Composition

data Render v k 
    = Render (Fix v) k

render v = Op $ inj $ Render v $ Pure ()