module PhasesFramework.Databind where
-- import Templates.Effects
-- import Actions.Effects
-- import Utils
-- import Definitions.GlobalVars.Denotation (Heap)
-- import Actions.Framework
-- import Templates.Syntax as S
-- import Actions.Syntax
-- import qualified Templates.Modules.Layout.Denotation as L
-- import qualified Templates.Modules.Render.Denotation as X
-- import qualified Templates.Modules.Page.PhasesDenotation as P
-- import Syntax (Address)
-- import qualified Templates.Modules.Lift.Denotation as Lt
-- import qualified Templates.Modules.Forms.PhasesDenotation as F
-- import Actions.Values (Lit)

-- type DbEff' v = State (Maybe LabelId) + Random Label LabelId +
--   State Seed + State FormId + ReqParamsSt + State Address
--   + MLState TVarAddress (Fix v) + Throw + EHeap v + Heap v + End

-- type Vt = Lit TVarAddress + PropRef + V'
-- type Vt' = Fix Vt

-- type RefEff = 


-- instance DenoteT Layout (Eff Vt) (DbEff' Vt) Vt' where
--   denoteT = L.denoteProcess

-- instance DenoteT S.Render (Eff Vt) (DbEff' Vt) Vt' where
--   denoteT = X.denoteProcess

-- instance DenoteT Page (Eff Vt) (DbEff' Vt) Vt' where
--   denoteT = P.denoteProcess

-- instance DenoteT (LiftT Stmt) (Eff Vt) (DbEff' Vt) Vt' where
--   denoteT = Lt.denote

-- instance DenoteT Forms(Eff Vt) (DbEff' Vt) Vt' where
--   denoteT = F.denoteProcess

-- instance DenoteT (Input (Fix Module)) (Eff Vt) (DbEff' Vt) Vt' where
--   denoteT = F.denoteDb

-- instance Lift (Eff Vt) (DbEff' Vt) Vt' where
--     lift = _

-- instance Lift (Heap Vt + End) (DbEff' Vt) Vt' where
--     lift = _

-- instance Denote 