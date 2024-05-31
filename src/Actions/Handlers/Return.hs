module Actions.Handlers.Return where

import Utils.Handler
import Actions.Effects
import Data.List (find)

funReturn :: Functor remEff => Handler (Abort val) val remEff val
funReturn = Handler
  { ret = pure
  , hdlr = \(Abort v) -> pure v }