module Arith.Interface where
import Effects (Operation (Oper), binaryOp)
import Arith.Syntax (Arith (..), OpArith, LitAr (Lit))
import Utils.Denote
import Utils.Composition
import Utils.Free (Free (Op, Pure))
import Prelude hiding (LT)

instance Functor eff => Denote 
    Arith (Operation OpArith LitAr + eff) LitAr 
    where
  denote :: Arith (Env -> Free (Operation OpArith LitAr + eff) LitAr) 
    -> Env -> Free (Operation OpArith LitAr + eff) LitAr
  denote (LitAr int) env = return int

  denote (OpArith op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'
