{-# OPTIONS_GHC -Wno-missing-fields #-}
module Actions.Framework where

import Actions.Effects
import Syntax
import Utils
import Actions.Modules.Arith.Syntax as A
import Actions.Modules.Bool.Syntax as B
import Actions.Modules.Col.Syntax as C
import Actions.Modules.Str.Syntax as Str
import Actions.Syntax
import Actions.Handlers.Return
import Actions.Handlers.Cond (condition)
import Actions.Handlers.Heap
import Actions.Modules.Arith.Denotation as A
import Actions.Modules.Bool.Denotation as B 
import Actions.Modules.Col.Denotation as C
import Actions.Modules.Entity.Denotation as En 
import Actions.Modules.Eval.Denotation as Ev 
import Actions.Modules.Expr.Denotation as Ex
import Actions.Modules.Fun.Denotation as F 
import Actions.Modules.Stmt.Denotation as S

import Actions.Modules.Str.Denotation as Str
import qualified Actions.Modules.Stmt.Denotation as St
import Actions.Handlers.Entity (uuidH, eHeapH)
import Actions.Values
import Templates.Modules.Lift.Syntax

type Eff = EffV V'
type EffV v   =  Cond + Abort (Fix v)
  + MLState Address (Fix v) + End
type V'      =  [] + LitBool + LitInt + LitStr + Null
type V       = Fix V'
type ModuleV = Col + Arith + Boolean + Str
type Module  = Weaken Loop + Stmt + Fun + Eval + Expr + ModuleV
type Out     = V --todo: make different!

runExp :: FreeEnv Eff V -> Out
runExp e = run e (Env { varEnv = []}) []

run :: FreeEnv Eff V -> Env Eff V -> [(Address, V)]
  -> Out
run e env store = unwrap
    $ handle_ heap' (makeEnv store)
    $ handle funReturn
    $ handle condition
    $ e env


instance (Lit Int <: v) => Denote Arith (EffV v) (Fix v) where
  denote = A.denote

instance (Lit Bool <: v) => Denote Boolean (EffV v) (Fix v) where
  denote = B.denote


instance (Null <: v) => Denote Eval (EffV v) (Fix v) where
  denote = Ev.denote

instance Denote Stmt (EffV v) (Fix v) where
  denote = S.denote

instance (Lit Bool <: v, [] <: v, Lit Int <: v, Null <: v, Eq (v (Fix v))) 
  => Denote Col (EffV v) (Fix v) where
  denote = C.denote

instance (Lit Int <: v, Lit Bool <: v, Eq (v (Fix v))) 
  => Denote Expr (EffV v) (Fix v) where
  denote = Ex.denote

instance (LitStr <: v, LitInt <: v) => Denote Str (EffV v) (Fix v) where
  denote = Str.denote

instance (Null <: v, [] <: v, Lit Bool <: v, Lit Int <: v) 
  => Denote (Weaken Loop) (EffV v) (Fix v) where
  denote = St.denoteLoop

instance (Null <: v) => Denote Fun (EffV v) (Fix v) where
  denote = F.denote
