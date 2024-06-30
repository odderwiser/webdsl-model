module Actions.Handlers.Return where

import Utils.Handler
import Actions.Effects
import Data.List (find)
import qualified Templates.Effects as E
import Definitions.Pages.Syntax
import Utils (Fix, BiFix)

funReturn :: Functor remEff => Handler (Abort v) v remEff v
funReturn = Handler
  { ret = pure
  , hdlr = \(Abort v) -> pure v }

funReturn' :: Functor remEff => Handler (Abort v) w remEff (Either v w )
funReturn' = Handler
  { ret = pure . Right
  , hdlr = \(Abort v) -> pure $ Left v }

dummyRedirect :: Functor g => Handler (E.Redirect v) value g value
dummyRedirect = Handler
  { ret = pure
  , hdlr = \(E.Redirect name args k) -> k
  }

redirectH :: Functor remEff => Handler_ (E.Redirect v) value (Maybe (PageCall (BiFix h (Fix g))(Fix g))) remEff (value, (Maybe (PageCall (BiFix h (Fix g))(Fix g))))
redirectH = Handler_
  { ret_ = curry pure
  , hdlr_ = \eff out -> case (eff, out) of
      ((E.Redirect name [] k), Nothing) -> k $ Just $ PCall name [] 
      ((E.Redirect name [] k), Just _) -> k $ out 
  }