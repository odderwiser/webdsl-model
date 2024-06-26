module Actions.Handlers.Cond where

import Utils.Handler
import Actions.Effects   (Cond (..))

import Data.Bits (xor)

condition :: (Functor g) => Handler Cond a g a
condition = Handler
  { ret = pure
  , hdlr = \(Cond v thenC elseC) ->
    if v then thenC else elseC }

convolution :: (Functor g) => Handler_ Cond a ([Bool], [Bool]) g (a, [Bool])
convolution = Handler_
  { ret_ = \output (message, code) -> pure (output, code)
  , hdlr_ = \op (msg, code) -> case (op, msg) of
    (Cond v thenC elseC, [b, b']) -> 
      let g = xor v $ xor b b'
          g' = xor v b'
          res = ([v, b], code ++ [g, g'])
      in if g then thenC res else elseC res }