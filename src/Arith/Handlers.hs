module Arith.Handlers where
import Effects (Operation (OpAr))
import Arith.Syntax (OpArith (..), LitAr (Lit))
import Utils.Handler
 
binOp :: (Functor g) => Handler (Operation OpArith) a g a
binOp = Handler
   { ret = pure
  , hdlr = \ case
      OpAr op [Lit v1, Lit v2] k -> k $ Lit $ calcArith op v1 v2
      }

calcArith :: OpArith -> Int -> Int -> Int
calcArith bop v1 v2 = do
    case bop of
        Add -> (+) v1 v2
        Div -> div v1 v2
        Sub -> (-) v1 v2
        Mul -> (*) v1 v2
        Mod -> mod v1 v2
