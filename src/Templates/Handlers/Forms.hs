module Templates.Handlers.Forms where
import Utils
import Templates.Effects (State (..))
import Actions.Effects (Random (Random))
import qualified Codec.Binary.UTF8.String as S
import Data.UUID.V3 (namespaceOID, generateNamed)
import Data.UUID (toString)
import Data.Char (isAlphaNum)

singleAccessState :: (Functor remEff) =>
  Handler_ (State a) val (Maybe a) remEff val
singleAccessState = Handler_ {
  ret_ = \val rep -> pure val,
  hdlr_= \eff box -> case (eff, box) of
    (GetS k, Just v) -> k v Nothing
    (PutS v k, box) -> k (Just v) 
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
