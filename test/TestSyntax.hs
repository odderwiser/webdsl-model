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
ifSyntax = if' false (int 1) (int 2)

ifComparison :: Fix (Arith + Boolean)
ifComparison = if' (B.or false true)
  (int 1) (int 2)

-- ARITH + BOOLEAN + EXPR

type AEB = Arith + Boolean + Expr

eqSyntax :: Fix AEB
eqSyntax = if' 
  (eq  true false) 
  (eq  true true) -- wrong on purpose
  (neq (int 1) (int 2))

cmpSyntax :: Fix AEB
cmpSyntax = if' 
  (lt  (int 1) (int 2)) 
  (lte (int 3) (int 3)) 
  (gt  (int 1) (int 1))

