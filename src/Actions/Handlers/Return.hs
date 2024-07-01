module Actions.Handlers.Return where

import Utils.Handler
import Actions.Effects
import Data.List (find)
import qualified Templates.Effects as E
import Definitions.Pages.Syntax
import Utils
import Syntax as S
import Actions.Arith as A
import Actions.Values 
import Actions.Modules.Bool.Syntax
import qualified Actions.Modules.Phases.Syntax as S

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

redirectH :: (Functor remEff, v~ Fix v'
  , LitInt <: v', LitBool <: v', Lit Uuid <: v'
  , Arith <: g, Boolean <:g, S.Ref <: g) 
  => Handler_ (E.Redirect v) value (Maybe (PageCall (BiFix h (Fix g))(Fix g))) 
  remEff (value, Maybe (PageCall (BiFix h (Fix g))(Fix g)))
redirectH = Handler_
  { ret_ = curry pure
  , hdlr_ = \eff out -> case (eff, out) of
      (E.Redirect name [] k, Nothing) -> k $ Just $ PCall name []
      (E.Redirect name _ k, Just _) -> k $ out
      (E.Redirect name arguments k, _) -> 
        k $ Just $ PCall name $ map mapToProgram arguments

  }

mapToProgram (v, Int) = (int $ unbox v, Int)
mapToProgram (v, Bool) = (injF $ LitB $ unbox v, Bool)
mapToProgram (v, Entity e ) = (injF $ S.Ref $ unbox v, Entity e)