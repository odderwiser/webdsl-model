module Bool.Handlers where
import Utils.Handler
import Effects (Operation (..), Cond (..))
import Bool.Syntax

binOp :: (Functor g) => Handler (Operation OpB LitB) a g a
binOp = Handler
   { ret = pure
  , hdlr = \case
        Oper op [Lit v1, Lit v2] k -> k $ Lit $ calcBool op v1 v2
      }

calcBool :: OpB -> Bool -> Bool -> Bool
calcBool bop v1 v2 = do
    case bop of
        And -> (&&) v1 v2
        Or  -> (||) v1 v2

condition :: (Functor g) => Handler Cond a g a
condition = Handler
  { ret = pure
  , hdlr = \(Cond (Lit v) thenC elseC) ->
    if v then thenC else elseC }
