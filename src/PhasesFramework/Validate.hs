module PhasesFramework.Validate where
import Utils
import Templates.Effects
import Actions.Effects
import Syntax
import Definitions.GlobalVars.Denotation (Heap)
import Definitions.GlobalVars.Effects (DbWrite, DbRead)
import Actions.Syntax
import Templates.Syntax as S hiding (Validate)
import Actions.FrameworkIO
import qualified Templates.Modules.Forms.PhasesDenotation as F
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Page.PhasesDenotation as P
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Layout.Denotation as L
import Templates.FrameworkIO as T
import Actions.Handlers.Entity (uuidH, WriteOps, eHeapH, dbWriteH, mockDbReadH, openDatabase, inMemoryDbReadH, Elems (entities), encodeElems, inMemoryDbReadH', dbReadIoH)
import Actions.Handlers.Return (funReturn, dummyRedirect)
import Actions.Handlers.Cond (condition)
import Definitions.Templates.Syntax
import qualified Templates.Modules.Page.Denotation as F
import qualified Templates.Modules.Page.PhasesDenotation as PF
import Actions.Values
import Definitions.Pages.Syntax (PageDef)
import Definitions.Fun.Syntax (FDecl)
import Definitions.Entity.Syntax
import Definitions.Pages.Framework (Eff'')
import qualified Definitions.Pages.Denotation as D
import qualified Definitions.Fun.Denotation as F
import qualified Definitions.Entity.Denotation as E
import qualified Definitions.Templates.Denotation as T
import Actions.Modules.Arith.Syntax (LitInt)
import Data.Aeson (ToJSON, FromJSON)
import Actions.Str (LitStr)
import Actions.Modules.Bool.Syntax (LitBool)
import PhasesFramework.Program
import Templates.Handlers.Forms
import Templates.Handlers.Env (paramsH)
import qualified Data.Map as Map
import Templates.Handlers.Layout
import PhasesFramework.Handlers
import Actions.Handlers.Heap
import Actions.Modules.Phases.Syntax (VTuple)
import qualified Actions.Modules.Phases.Denotation as V
import qualified Data.Aeson.KeyMap as KM
import Templates.Modules.Phases.Denotation (denoteA)
import qualified Templates.Modules.Lift.Denotation as L

type VEff' v = Validate + State FormId + State Seed
  + Random Label LabelId + State (Maybe LabelId)
  + State TVarSeed + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Writer (TId, String) + Reader () TId + Throw
  + Writer String 
  + EHeap v + Heap v + DbRead (EntityDecl (Fix v)) + DbWrite (Fix v)+ End -- can I get rid of this read and write?

vH :: Functor g => Handler Validate a g a
vH = Handler { ret = pure }

type VIn v = ([(Address, Fix v)], String, [(String, String)], [(TVarAddress, Fix v)], [TId], [(Uuid, EntityDecl (Fix v))])

executeVPhase :: forall v. (ToJSON (v(Fix v)), FromJSON (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, V' <<: v, WeakenF v V', Show (v (Fix v)))
  => Free (VEff' v) () ->  VIn v -> IO [(TId, String)]
executeVPhase e (heap, file, params, cache, tIds, eCache) = do
  (status, elems :: Elems V') <- openDatabase file
  let elems' :: Elems v = elems {entities = KM.map (fmap cmapF) $ entities elems}
  writeFile file $ encodeElems elems'
  action <- ioHandle (dbReadIoH file)
        $ handle_ heap' (makeEnv heap)
        $ handle_ eHeapH eCache
        $ handle_ appendWriterH []
        $ handle mockThrowH -- throw (effect to remove)
        $ handle_ consumingReaderH tIds
        $ handle_ appendWriterH []
        $ handle_ cacheH' (Map.fromList cache)
        $ handle_ stateElH Nothing -- state address
        $ handle_ paramsH (Map.fromList params) -- reqparamsst
        $ handle_ autoIncrementState (VSeed 0) -- state tvarseed
        $ handle_ singleAccessState Nothing --state maybe labelid
        $ handle idH -- random label labelid
        $ handle_ autoIncrementState (Seed 0) -- state seed
        $ handle_ simpleStateH "" --state formid
        $ handle vH     --databind
        $ e
  (((_, errors), log), readstatus) <-  unwrap
        $ handle_ (dbWriteH file) ([] :: [WriteOps v]) $ action
  (status, elemsV :: Elems v) <- openDatabase file
  let elems''' :: Elems V' = elems {entities = KM.map (fmap weakenF) $ entities elemsV}
  writeFile file $ encodeElems elems'''
  return errors

data Validate k
  deriving Functor

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

instance DenoteT  (LiftE VTuple) (EffV Vt) (VEff' Vt) Vt' where
  denoteT = V.denoteT

instance DenoteT TBody (EffV Vt) (VEff' Vt) Vt' where
  denoteT = F.denoteBody

instance DenoteT EvalT (EffV Vt) (VEff' Vt) Vt' where
  denoteT = PF.denoteEProcess

instance DenoteT Action (EffV Vt) (VEff' Vt) Vt' where
  denoteT = denoteA

instance DenoteT Loop (EffV Vt) (VEff' Vt) Vt' where
  denoteT = L.denoteT

instance Lift (EffV Vt) (VEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle dummyRedirect
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e

instance DenoteDef' PageDef (PEnv (EffV Vt) (VEff' Vt) Vt') (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (VEff' Vt) Vt) where
  denoteDef' = D.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV Vt) Vt') (Eff''  (EffV Vt) (VEff' Vt) Vt) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (VEff' Vt) Vt) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV Vt) (VEff' Vt) Vt') (FreeEnv (EffV Vt) Vt')(Eff''  (EffV Vt) (VEff' Vt) Vt) where
  denoteDef'= T.denoteDefT