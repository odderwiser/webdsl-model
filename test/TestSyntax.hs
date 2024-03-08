module TestSyntax where
import Arith.Syntax as A
import Utils.Fix
import Utils.Composition (type (+))
import Bool.Syntax as B

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

