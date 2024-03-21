module Bool.Handlers where
import Utils.Handler
import Bool.Effects   (Cond (..))
import Bool.Syntax    ( Boolean(OpB), OpB(..))

condition :: (Functor g) => Handler Cond a g a
condition = Handler
  { ret = pure
  , hdlr = \(Cond v thenC elseC) ->
    if v then thenC else elseC }
