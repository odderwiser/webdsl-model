module Actions.FrameworkIO where

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
import Actions.Handlers.Entity (uuidH, eHeapH, mockDbReadH, dbWriteH, WriteOps, DbStatus)
import Definitions.GlobalVars.Syntax (Uuid)
import Actions.Values
import Definitions.GlobalVars.Effects (DbRead, DbWrite)

type Eff = EffV V'
type EffV v    =  Cond + Abort (Fix v) + Random String String 
  + EHeap v + MLState Address (Fix v) + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) +  End
type V'      =  [] + LitBool + LitInt + LitStr + Null + EntityDecl + Lit Address + Lit Uuid
type V       = Fix V'
type ModuleV = Col + Arith + Boolean + Str
type Module  = EntityDecl + Entity + Loop + Stmt + Fun + Eval + Expr + ModuleV
type Out     = V --todo: make different!

runExp :: FreeEnv Eff V -> String -> IO (Out, DbStatus)
runExp e = run e (Env { varEnv = []}) [] 

run :: FreeEnv Eff V -> Env Eff V -> [(Address, V)] -> String
  -> IO (Out, DbStatus)
run e env store file = unwrap
    $ handle mockDbReadH
    $ handle_ (dbWriteH file) ([] :: [WriteOps V']) 
    $ handle_ heap' (makeEnv store)
    $ handle_ eHeapH []
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    $ e env 
  -- return out
  


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

instance (LitStr <: v) => Denote Str (EffV v) (Fix v) where
  denote = Str.denote

instance (Null <: v, [] <: v, Lit Bool <: v, Lit Int <: v) 
  => Denote Loop (EffV v) (Fix v) where
  denote = St.denoteLoop

instance (Null <: v) => Denote Fun (EffV v) (Fix v) where
  denote = F.denote

instance (Null <: v, Lit Uuid <: v) 
  => Denote Entity (EffV v) (Fix v) where
  denote = En.denote

instance (Lit Address <: v, Lit Uuid <: v, Null <: v, Show  (v(Fix v))) 
  => Denote EntityDecl (EffV v) (Fix v) where
  denote = En.denoteEDecl