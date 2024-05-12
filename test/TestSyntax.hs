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
  $ If (injB False) (injB False) (injB True)

ifComplicated :: Fix Boolean
ifComplicated = injF $ If 
  (injF $ OpB And (injB False) (injB True))
  (injB False) 
  (injB True)

-- ARITH + BOOLEAN

ifSyntax :: Fix (Arith + Boolean)
ifSyntax = injF 
  $ If (injB False) (injA 1) (injA 2)

ifComparison :: Fix (Arith + Boolean)
ifComparison = injF 
  $ If (injF 
    $ OpB Or (injB False) (injB True))
  (injA 1) (injA 2)
-- ARITH + BOOLEAN + EXPR

type AEB = Arith + Boolean + Expr

eqSyntax :: Fix AEB
eqSyntax = injF $ If 
  (injF $ OpCmp Eq (injB True, Bool) (injB False, Bool)) 
  (injF $ OpCmp Eq (injB True, Bool) (injB True, Int)) -- wrong on purpose
  (injF $ OpCmp Neq (injA 1, Int) (injA 2, Int))

cmpSyntax :: Fix AEB
cmpSyntax = injF $ If 
  (injF $ OpCmp Lt  (injA 1, Int) (injA 2, Int)) 
  (injF $ OpCmp Lte (injA 3, Int) (injA 3, Int)) 
  (injF $ OpCmp Gt  (injA 1, Int) (injA 1, Int))

