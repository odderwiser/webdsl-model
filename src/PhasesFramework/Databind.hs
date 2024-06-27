{-# OPTIONS_GHC -Wno-missing-fields #-}
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
import Actions.Handlers.Entity (uuidH, eHeapH, WriteOps, mockDbReadH)
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
import PhasesFramework.Program
import Data.Aeson (ToJSON, FromJSON)
import Actions.Str (LitStr)
import Actions.Modules.Arith.Syntax (LitInt)
import Actions.Modules.Bool.Syntax (LitBool)
import Templates.Handlers.Render as R

type DbEff' v = Databind + State (Maybe LabelId) + Random Label LabelId +
  State Seed + State FormId + State TVarSeed + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Throw 
  + EHeap v + Heap v + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) + End

data Databind k
  deriving Functor

dH :: Handler Databind a v a 
dH = Handler {
  ret = pure
}


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


runApplied :: (ToJSON (v(Fix v)), FromJSON (v (Fix v)), 
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => Free (DbEff' v) () -> String -> IO Out'
runApplied e file = do
  ((_, out), readstatus) <-  unwrap
    $ handle mockDbReadH
    $ handle_ (dbWriteH file) ([] :: [WriteOps v]) 
    $ handle_ heap' (makeEnv [])
    $ handle_ eHeapH []
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
    $ handle dH
    $ e 
  return out