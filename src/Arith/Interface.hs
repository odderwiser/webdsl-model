module Arith.Interface where
import Effects (Operation, binaryOp)
import Arith.Syntax (Arith (..), OpArith, LitAr (Lit))
import Utils.Denote
import Utils.Composition
import Utils.Free (Free)

instance (Operation OpArith (LitAr e) <:  eff) 
  => Denote Arith eff LitAr where
  denote :: (Operation OpArith (LitAr e) <: eff) 
    => Arith (Env -> Free eff (LitAr e1)) 
    -> Env 
    -> Free eff (LitAr e1)

  denote (LitAr e) env = return $ coerce e

  denote (OpArith op a b) env = do 
    a' <- a env 
    b' <- b env 
    binaryOp op (coerce a' :: LitAr e) (coerce b')
