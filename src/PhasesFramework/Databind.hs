module PhasesFramework.Databind where
import Templates.Effects
import Actions.Effects
import Utils
import Definitions.GlobalVars.Denotation (Heap)
import Actions.FrameworkIO
import Templates.FrameworkIO
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
import Definitions.GlobalVars.Effects (DbRead, DbWrite)
import Definitions.Templates.Syntax (TBody, TemplateDef)
import qualified Templates.Modules.Page.Denotation as F
import qualified Templates.Modules.Page.PhasesDenotation as PF
import Definitions.Pages.Syntax (PageDef)
import Definitions.Fun.Syntax (FDecl)
import Definitions.Entity.Syntax (EntityDef)
import Definitions.Pages.Framework (Eff'')
import qualified Definitions.Pages.Denotation as D
import qualified Definitions.Fun.Denotation as F
import qualified Definitions.Entity.Denotation as E
import qualified Definitions.Templates.Denotation as T

type DbEff' v = Databind + State (Maybe LabelId) + Random Label LabelId +
  State Seed + State FormId + State TVarSeed + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Throw 
  + EHeap v + Heap v + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) + End

type Vt = Lit TVarAddress + PropRef + V'
type Vt' = Fix Vt

data Databind k
  deriving Functor


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

instance DenoteT TBody (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = F.denoteBody

instance DenoteT EvalT (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = PF.denoteEDb

instance Lift (EffV Vt) (DbEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e
 
--

instance DenoteDef' PageDef (PEnv (EffV Vt) (DbEff' Vt) Vt') (FreeEnv (EffV Vt) Vt') (Eff'' (DbEff' Vt) Vt) where
  denoteDef' = D.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV Vt) Vt') (Eff'' (DbEff' Vt) Vt) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffV Vt) Vt') (Eff'' (DbEff' Vt) Vt) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV Vt) (DbEff' Vt) Vt') (FreeEnv (EffV Vt) Vt')(Eff'' (DbEff' Vt) Vt) where
  denoteDef'= T.denoteDefT