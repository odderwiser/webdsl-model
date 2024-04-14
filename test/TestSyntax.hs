module TestSyntax where
import Arith.Syntax as A
import Utils.Fix
import Utils.Composition (type (+))
import Bool.Syntax as B
import Expr.Syntax 
import Syntax (Type(..))


-- BOOLEAN 

ifSimple :: Fix Boolean
ifSimple = In 
    $ If (injF $ B.lit False)
        (injF $ B.lit False)
        (injF $ B.lit True)

ifComplicated :: Fix Boolean
ifComplicated = In
    $ If (In $ bin And
            (B.lit False)
            (B.lit True))
        (injF $ B.lit False)
        (injF $ B.lit True)

-- ARITH + BOOLEAN

ifSyntax :: Fix (Arith + Boolean)
ifSyntax = injF $ 
    If (injF (B.lit False))
        (injF (A.lit 1))
        (injF (A.lit 2))

ifComparison :: Fix (Arith + Boolean)
ifComparison = injF 
  $ If (injF (OpB
    Or
      (injF $ B.lit False)
      (injF $ B.lit True)))
    (injF (A.lit 1))
    (injF (A.lit 2))
-- ARITH + BOOLEAN + EXPR

type AEB = Arith + Boolean + Expr

eqSyntax :: Fix AEB
eqSyntax = injF 
    $ If (injF $ OpCmp Eq 
        (injF $ B.lit True, Bool) 
        (injF $ B.lit False, Bool)) 
        (injF $ OpCmp Eq 
            (injF $ B.lit True, Bool) 
            (injF $ B.lit True, Int)) -- wrong on purpose
        (injF $ OpCmp Neq 
            (injF $ A.lit 1, Int) 
            (injF $ A.lit 2, Int))

cmpSyntax :: Fix AEB
cmpSyntax = injF 
    $ If (injF $ OpCmp Lt
        (injF $ A.lit 1, Int) 
        (injF $ A.lit 2, Int)) 
        (injF $ OpCmp Lte 
            (injF $ A.lit 3, Int) 
            (injF $ A.lit 3, Int)) 
        (injF $ OpCmp Gt 
            (injF $ A.lit 1, Int) 
            (injF $ A.lit 1, Int))

