module Templates.Handlers.Forms where
import Utils
import Templates.Effects (State (..), Throw, Reader (ReadNext, Read))
import Actions.Effects (Random (Random), Writer (..))
import qualified Codec.Binary.UTF8.String as S
import Data.UUID.V3 (namespaceOID, generateNamed)
import Data.UUID (toString)
import Data.Char (isAlphaNum)
import Templates.Modules.Page.Syntax
import Data.List.Extra (snoc)
import qualified Data.Map as Map
import Data.Maybe (fromJust, fromMaybe)

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

mockThrowH :: Functor g => Handler Throw a g a
mockThrowH = Handler {
  ret = pure
}

templateIdMaybeReaderH :: (Functor g) => Handler_ (Reader () (Maybe TId)) v [String] g v
templateIdMaybeReaderH = Handler_
  { ret_   = \v input -> pure v
  , hdlr_ = \(ReadNext k ) input -> case input of
      [] -> k Nothing []
      (h : t)-> k (Just $ TId  h) t
  }

consumingReaderH :: (Functor g) => Handler_ (Reader () v) val [v] g val
consumingReaderH = Handler_
  { ret_ = \v ids -> pure v
  , hdlr_ = \(ReadNext k ) input ->  case input of
      h : t -> k h t
  }

nonConsumingReaderH ::  (Functor g, Ord k) => Handler_ (Reader k [v]) val (Map.Map k [v]) g val
nonConsumingReaderH = Handler_
  { ret_ = \v input -> pure v
  , hdlr_ = \(Read v k) input -> k (fromMaybe [] (Map.lookup v input)) input
  }

appendWriterH :: Functor g => Handler_ (Writer v) val [v] g (val, [v])
appendWriterH = Handler_
  { ret_ = curry pure
  , hdlr_ = \(Write v k) output -> k $ snoc output v
  }