module Templates.FrameworkIO where

import Templates.Effects as E
import Utils
import Templates.Modules.Attributes.Syntax
import Templates.Syntax as S
import Actions.FrameworkIO
import Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Return (funReturn, funReturn')
import Actions.Handlers.Cond (condition)
import Actions.Effects (MLState, Random)
import Syntax (Address)
import Actions.Handlers.Heap (heap, makeEnv, heap')
import Templates.Modules.Layout.Denotation as L
import Templates.Modules.Render.Denotation as X
import Templates.Modules.Page.Denotation as P
import Templates.Modules.Lift.Denotation as Lt
import Templates.Modules.Lift.Syntax (LiftT)
import Actions.Syntax (Stmt, Eval)
import Actions.Handlers.Entity (uuidH, eHeapH, WriteOps, dbWriteH, mockDbReadH)
import Templates.Modules.Forms.Denotation as F
import Templates.Handlers.Forms (singleAccessState, idH, autoIncrementState, simpleStateH)
import Actions.Modules.Entity.Syntax (Entity, EntityDecl)
import Definitions.Templates.Syntax (TBody)
import Actions.Values (Lit)
import Definitions.GlobalVars.Syntax (Uuid)
import Definitions.GlobalVars.Effects (DbWrite, DbRead)

type Eff' = Eff'V V'  
type Eff'V v = State ButtonCount + State FormId + State Seed + Random Label LabelId + State (Maybe LabelId) 
  + Attribute + Stream HtmlOut + State AttList + E.Render v + State Address 
  + MLState Address (Fix v) + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) +  End
type T = Input (Fix Module) +: Forms +: Layout +: S.Render +: Page +: LiftT Stmt +: TBody +: EvalT
--running syntax
type Module' = BiFix T (Fix Module)   
type Out' = String


run :: PEnv Eff Eff' V
  -> Out'
run e = runEnv e (TEnv { actionEnv = Env {}})

runEnv :: PEnv Eff Eff' V -> TEnv Eff Eff' V
  -> IO Out'
runEnv e env = runApplied (e env) ""

runApplied :: Free Eff' () -> String -> IO Out'
runApplied e file = do
  ((_, out), readstatus) <-  unwrap
    $ handle mockDbReadH
    $ handle_ (dbWriteH file) ([] :: [WriteOps V']) 
    $ handle_ heap' (makeEnv [])
    $ handle_ stateElH Nothing
    $ handle renderH
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { R.title = Nothing, body = "", pageCall = False})
    $ handle_ attributeH ("section", 1)
    $ handle_ singleAccessState Nothing
    $ handle idH
    $ handle_ autoIncrementState (Seed 0)
    $ handle_ simpleStateH ""
    $ handle_ autoIncrementState (Count 0)
    $ e 
  return out
 

instance (Lit Uuid <: v) => Lift (EffV v) (Eff'V v) (Fix v) where
  lift  = handleExp



handleExp :: Free (EffV v) (Fix v) -> Free (Eff'V v) (Fix v)
handleExp e = bubbleDown
  $ handle funReturn
  $ handle condition
  e
    

-- probably a beeter way to implement this??
bubbleDown ::
    (eff <<: eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance DenoteT Layout Eff Eff' V where
  denoteT :: Layout (PEnv Eff Eff' V) (FreeEnv Eff V) -> PEnv Eff Eff' V
  denoteT = L.denote

instance DenoteT S.Render Eff Eff' V where
  denoteT :: S.Render (PEnv Eff Eff' V) (FreeEnv Eff V) -> PEnv Eff Eff' V
  denoteT = X.denote

instance DenoteT Page Eff Eff' V where
  denoteT = P.denote

instance DenoteT (LiftT Stmt) Eff Eff' V where
  denoteT = Lt.denote

instance DenoteT Forms Eff Eff' V where
  denoteT = F.denoteR

instance DenoteT (Input (Fix Module)) Eff Eff' V where
  denoteT = F.denoteRInput

instance DenoteT TBody Eff Eff' V where
  denoteT = P.denoteBody

instance DenoteT EvalT Eff Eff' V where
  denoteT = P.denoteE