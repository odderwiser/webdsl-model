module PhasesFramework.Databind where
import Templates.Effects
import Actions.Effects
import Utils
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Framework
import Templates.Framework
import Templates.Syntax as S
import Actions.Syntax
import qualified Templates.Modules.Layout.Denotation as L
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Page.PhasesDenotation as P
import Syntax (Address)
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Forms.PhasesDenotation as F
import Actions.Values (Lit)
import qualified Actions.Modules.Entity.Denotation as En
import Actions.Handlers.Cond (condition)
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Entity (uuidH, eHeapH)

type DbEff' v = State (Maybe LabelId) + Random Label LabelId +
  State Seed + State FormId + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Throw + EHeap v + Heap v + End

type Vt = Lit TVarAddress + PropRef + V'
type Vt' = Fix Vt


instance DenoteT Layout (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = L.denoteProcess

instance DenoteT S.Render (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = X.denoteProcess

instance DenoteT Page (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = P.denoteProcess

instance DenoteT (LiftT Stmt) (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = Lt.denote

instance DenoteT Forms(EffV Vt) (DbEff' Vt) Vt' where
  denoteT = F.denoteProcess

instance DenoteT (Input (Fix Module)) (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = F.denoteDb

instance Lift (EffV Vt) (DbEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle_ eHeapH [] -- probably wrong?
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e
 
--

-- instance Denote EntityDecl (EffV Vt) Vt' where
--   denote = En.denoteEDecl