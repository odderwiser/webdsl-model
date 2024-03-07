module Bool.Handlers where
import Utils.Handler
import Effects (Operation (OpBool), Cond (..))
import Bool.Syntax ( Boolean(OpB), OpB(..), LitB(Lit) )

condition :: (Functor g) => Handler Cond a g a
condition = Handler
  { ret = pure
  , hdlr = \(Cond (Lit v) thenC elseC) ->
    if v then thenC else elseC }
