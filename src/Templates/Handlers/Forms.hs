module Templates.Handlers.Forms where
import Utils
import Templates.Effects (State (..))
import Actions.Effects (Random (Random))
import qualified Codec.Binary.UTF8.String as S
import Data.UUID.V3 (namespaceOID, generateNamed)
import Data.UUID (toString)
import Data.Char (isAlphaNum)

singleAccessState :: (Functor remEff) =>
  Handler_ (State (Maybe a)) val (Maybe a) remEff val
singleAccessState = Handler_ {
  ret_ = \val rep -> pure val,
  hdlr_= \eff box -> case (eff, box) of
    (GetS k, _) -> k box Nothing
    (PutS v k, box) -> k v 
}

autoIncrementState :: (Functor remEff, Num a) =>
  Handler_ (State a) val a remEff val
autoIncrementState = Handler_ {
  ret_ = \val rep -> pure val,
  hdlr_= \eff box -> case (eff, box) of
    (GetS k, i) -> k i (i+1)
    (PutS v k, box) -> k v 
}

idH :: (Functor eff)
  => Handler (Random String String) val eff val
idH = Handler
  { ret  = pure
  , hdlr = \(Random obj k) ->
      k $ filter isAlphaNum 
        $ toString 
        $ generateNamed namespaceOID 
        $ S.encode obj
  }

simpleStateH :: (Functor remEff) => Handler_ (State v) val v remEff val
simpleStateH = Handler_ 
  { ret_ = \v val -> pure v
  , hdlr_ = \eff val -> case eff of 
      (GetS k) -> k val val
      (PutS v k) -> k v
  }