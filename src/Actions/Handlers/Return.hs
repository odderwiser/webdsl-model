module Actions.Handlers.Return where

import Utils.Handler
import Fun.Effects
import Eval.Effects
import Fun.Syntax
import Utils.Environment as U
import Data.List (find)
import Utils.Free

funReturn :: Functor remEff => Handler (Abort val) val remEff val
funReturn = Handler
  { ret = pure
  , hdlr = \(Abort v) -> pure v }