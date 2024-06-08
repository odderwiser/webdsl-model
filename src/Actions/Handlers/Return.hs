module Actions.Handlers.Return where

import Utils.Handler
import Actions.Effects
import Data.List (find)

funReturn :: Functor remEff => Handler (Abort v) v remEff v
funReturn = Handler
  { ret = pure
  , hdlr = \(Abort v) -> pure v }

funReturn' :: Functor remEff => Handler (Abort v) w remEff (Either v w )
funReturn' = Handler
  { ret = pure . Right
  , hdlr = \(Abort v) -> pure $ Left v }