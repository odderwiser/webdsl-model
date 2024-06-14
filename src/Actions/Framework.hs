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
import Definitions.GlobalVars.Syntax (Uuid)
import Actions.Values

type Eff     =  Cond + Abort V + Random String String + EHeap V' + MLState Address V + End
type V'      =  [] + LitBool + LitInt + LitStr + Null + EntityDecl + Lit Address + Lit Uuid
type V       = Fix V'
type ModuleV = Col + Arith + Boolean + Str
type Module  = EntityDecl + Entity + Loop + Stmt + Fun + Eval + Expr + ModuleV
type Out     = V --todo: make different!

runExp :: FreeEnv Eff V -> Out
runExp e = run e (Env { varEnv = []}) []

run :: FreeEnv Eff V -> Env Eff V -> [(Address, V)]
  -> Out
run e env store = unwrap
    $ handle_ heap' (makeEnv store)
    $ handle_ eHeapH []
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    $ e env


instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote


instance Denote Eval Eff V where
  denote = Ev.denote

instance Denote Stmt Eff V where
  denote = S.denote

instance Denote Col Eff V where
  denote = C.denote

instance Denote Expr Eff V where
  denote = Ex.denote

instance Denote Str Eff V where
  denote = Str.denote

instance Denote Loop Eff V where
  denote = St.denoteLoop

instance Denote Fun Eff V where
  denote = F.denote

instance Denote Entity Eff V where
  denote = En.denote

instance Denote EntityDecl Eff V where
  denote = En.denoteEDecl