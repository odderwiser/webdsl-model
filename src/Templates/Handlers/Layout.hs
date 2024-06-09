module Templates.Handlers.Layout where
import Utils
import Templates.Effects
import Templates.Modules.Attributes.Syntax

attributeH :: (Functor remEff) =>
  Handler_ Attribute val (String, Int)
  remEff val
attributeH = Handler_ {
  ret_ = \x rep -> pure x
  ,hdlr_ = \eff (name, v) -> case eff of
    (Increment k) -> k (name, v+1)
    (Decrement k) -> k (name, v-1)
    (Get k)       -> k (name++ show v) (name, v)
}

stateH :: (Functor remEff) =>
  Handler_ (State AttList) val AttList remEff val
stateH = Handler_ {
  ret_ = \val rep -> pure val,
  hdlr_= \eff atts -> case eff of
    (GetS k) -> k atts []
    (PutS v k) -> k $ v ++ atts
}

stateElH :: (Functor remEff) =>
  Handler_ (State a) val (Maybe a) remEff val
stateElH = Handler_ {
  ret_ = \val rep -> pure val,
  hdlr_= \eff box -> case (eff, box) of
    (GetS k, Just v) -> k v box
    (PutS v k, box) -> k (Just v) 
}