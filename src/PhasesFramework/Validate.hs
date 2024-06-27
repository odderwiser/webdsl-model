module PhasesFramework.Validate where
import Utils
import Templates.Effects
import Actions.Effects
import Syntax
import Definitions.GlobalVars.Denotation (Heap)
import Definitions.GlobalVars.Effects (DbWrite, DbRead)
import Actions.Syntax
import Templates.Syntax as S
import Actions.FrameworkIO
import qualified Templates.Modules.Forms.PhasesDenotation as F
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Page.PhasesDenotation as P
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Layout.Denotation as L
import Templates.Framework (bubbleDown)
import Actions.Handlers.Entity (uuidH)
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Cond (condition)
import Definitions.Templates.Syntax
import qualified Templates.Modules.Page.Denotation as F
import qualified Templates.Modules.Page.PhasesDenotation as PF
import Actions.Values
    
type VEff' v = State (Maybe LabelId) + Random Label LabelId +
  State Seed + State FormId + ReqParamsSt + State Address + State TVarSeed
  + MLState TVarAddress (Fix v) + Throw 
  + EHeap v + Heap v + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) + End -- can I get rid of this read and write?

type Vt = Lit TVarAddress + PropRef + 
    V'
type Vt' = Fix Vt


instance DenoteT Layout (EffV Vt) (VEff' Vt) Vt' where
  denoteT = L.denoteProcess

instance DenoteT S.Render (EffV Vt) (VEff' Vt) Vt' where
  denoteT = X.denoteProcess

instance DenoteT Page (EffV Vt) (VEff' Vt) Vt' where
  denoteT = P.denoteProcess

instance DenoteT (LiftT Stmt) (EffV Vt) (VEff' Vt) Vt' where
  denoteT = Lt.denote

instance DenoteT Forms(EffV Vt) (VEff' Vt) Vt' where
  denoteT = F.denoteProcess

instance DenoteT (Input (Fix Module)) (EffV Vt) (VEff' Vt) Vt' where
  denoteT = F.denoteV

instance DenoteT TBody (EffV Vt) (VEff' Vt) Vt' where
  denoteT = F.denoteBody

instance DenoteT EvalT (EffV Vt) (VEff' Vt) Vt' where
  denoteT = PF.denoteEProcess

instance Lift (EffV Vt) (VEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e