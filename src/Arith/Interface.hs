module Arith.Interface where
import Effects (Operation, binaryOp)
import Arith.Syntax (Arith (..), OpArith, LitAr (Lit))
import Utils.Denote
import Utils.Composition
import Utils.Free (Free)

instance (Operation OpArith (LitAr e) <:  eff) 
  => Denote Arith eff (LitAr e) where
  denote :: Functor eff =>
    Arith (Env -> Free eff (LitAr e))
    -> Env -> Free eff (LitAr e)
    
  denote (LitAr (Lit int)) env = return (Lit int)

  denote (OpArith op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op a' b'
