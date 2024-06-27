module PhasesFramework.Action where
import Templates.Effects
import Actions.Effects
import Utils
import Syntax
import Definitions.GlobalVars.Denotation (Heap)
import Definitions.GlobalVars.Effects
import Actions.Syntax
import Actions.Values (Lit)
import Actions.FrameworkIO
import Templates.Syntax as S
import Definitions.Templates.Syntax (TBody)
import Templates.FrameworkIO
import Actions.Handlers.Entity (uuidH)
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Cond (condition)
import qualified Templates.Modules.Layout.Denotation as L
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Page.PhasesDenotation as P
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Forms.PhasesDenotation as F

type AEff' v = State (Maybe LabelId) + Random Label LabelId 
  + State ButtonCount + State Seed + State FormId + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Throw 
  + EHeap v + Heap v + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) + End -- can I get rid of this read and write?

type Vt = Lit TVarAddress + PropRef + 
    V'
type Vt' = Fix Vt


instance DenoteT Layout (EffV Vt) (AEff' Vt) Vt' where
  denoteT = L.denoteProcess

instance DenoteT S.Render (EffV Vt) (AEff' Vt) Vt' where
  denoteT = X.denoteProcess

instance DenoteT Page (EffV Vt) (AEff' Vt) Vt' where
  denoteT = P.denoteProcess

instance DenoteT (LiftT Stmt) (EffV Vt) (AEff' Vt) Vt' where
  denoteT = Lt.denote

instance DenoteT Forms(EffV Vt) (AEff' Vt) Vt' where
  denoteT = F.denoteAction

instance DenoteT (Input (Fix Module)) (EffV Vt) (AEff' Vt) Vt' where
  denoteT = F.denoteA

instance DenoteT TBody (EffV Vt) (AEff' Vt) Vt' where
  denoteT = F.denoteBody

instance DenoteT EvalT (EffV Vt) (AEff' Vt) Vt' where
  denoteT = PF.denoteEProcess

instance Lift (EffV Vt) (AEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e