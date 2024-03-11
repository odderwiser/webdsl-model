module Eval.Handlers where
import Utils.Handler (Handler_ (..))
import Eval.Effects (MLState (Deref, Assign))

environment :: (Functor g, Ord m) 
    => Handler_ (MLState m v) a [(m, v)] g (a, [(m, v)])
environment = Handler_
  { ret_ = \x map -> pure (x, map)
  , hdlr_ = \x map -> case x of
      Deref key k -> k (case lookup key map of 
            Just x -> x) map
      Assign (key, value) k -> k $ (key, value) : map}
