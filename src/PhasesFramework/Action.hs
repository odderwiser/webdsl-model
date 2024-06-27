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
import Definitions.Templates.Syntax (TBody, TemplateDef)
import Templates.FrameworkIO
import Actions.Handlers.Entity (uuidH)
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Cond (condition)
import qualified Templates.Modules.Layout.Denotation as L
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Page.PhasesDenotation as P
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Forms.PhasesDenotation as F
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

type AEff' v = ActionE + State (Maybe LabelId) + Random Label LabelId + State TVarSeed
  + State ButtonCount + State Seed + State FormId + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Throw 
  + EHeap v + Heap v + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) + End -- can I get rid of this read and write?

type Vt = Lit TVarAddress + PropRef + 
    V'
type Vt' = Fix Vt

data ActionE k 
  deriving Functor

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

instance DenoteDef' PageDef (PEnv (EffV Vt) (AEff' Vt) Vt') (FreeEnv (EffV Vt) Vt') (Eff'' (AEff' Vt)  Vt) where
  denoteDef' = D.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV Vt) Vt') (Eff'' (AEff' Vt) Vt) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffV Vt) Vt') (Eff'' (AEff' Vt) Vt) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV Vt) (AEff' Vt) Vt') (FreeEnv (EffV Vt) Vt')(Eff'' (AEff' Vt) Vt) where
  denoteDef'= T.denoteDefT