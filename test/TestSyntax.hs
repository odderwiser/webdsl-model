module TestSyntax where
import Actions.Arith as A
import Utils.Fix
import Utils.Composition (type (+))
import Actions.Bool as B
import Actions.Syntax 
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
  (injF $ OpCmp Eq  (injB True) (injB False)) 
  (injF $ OpCmp Eq  (injB True) (injB True)) -- wrong on purpose
  (injF $ OpCmp Neq (injA 1)    (injA 2))

cmpSyntax :: Fix AEB
cmpSyntax = injF $ If 
  (injF $ OpCmp Lt  (injA 1) (injA 2)) 
  (injF $ OpCmp Lte (injA 3) (injA 3)) 
  (injF $ OpCmp Gt  (injA 1) (injA 1))

