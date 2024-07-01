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
import Actions.Handlers.Entity (uuidH, WriteOps, eHeapH, dbWriteH, mockDbReadH, inMemoryDbReadH, openDatabase, Elems (entities), encodeElems)
import Actions.Handlers.Return (funReturn, redirectH)
import Actions.Handlers.Cond (condition)
import qualified Templates.Modules.Layout.Denotation as L
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Page.PhasesDenotation as P
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Forms.PhasesDenotation as F
import qualified Templates.Modules.Page.Denotation as F
import qualified Templates.Modules.Page.PhasesDenotation as PF
import Definitions.Pages.Syntax (PageDef, PageCall)
import Definitions.Fun.Syntax (FDecl)
import Definitions.Entity.Syntax (EntityDef)
import Definitions.Pages.Framework (Eff'')
import qualified Definitions.Pages.Denotation as D
import qualified Definitions.Fun.Denotation as F
import qualified Definitions.Entity.Denotation as E
import qualified Definitions.Templates.Denotation as T
import PhasesFramework.Program
import Data.Aeson (ToJSON)
import Data.Aeson.Types (FromJSON)
import Actions.Str (LitStr)
import Actions.Arith
import Actions.Bool (LitBool)
import Templates.Handlers.Forms
import Templates.Handlers.Env
import qualified Data.Map as Map
import Templates.Handlers.Layout
import PhasesFramework.Handlers (cacheH)
import Actions.Handlers.Heap
import qualified Data.Aeson.KeyMap as KM
import Templates.Modules.Phases.Denotation (denoteA, denoteAAction)
import Definitions.GlobalVars.Syntax (Uuid)

type AEff' v = ActionE + State FormId + State Seed 
  + Random Label LabelId + State (Maybe LabelId) + State TVarSeed
  + State ButtonCount +  ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Throw + Redirect (Fix v) + Writer String
  + EHeap v + Heap v + DbRead (EntityDecl (Fix v)) + DbWrite (Fix v) + End -- can I get rid of this read and write?

data ActionE k 
  deriving Functor

aH :: Functor g => Handler ActionE a g a
aH = Handler { ret = pure }  

type AIn v = ([(Address, Fix v)], String, [(String, String)], [(TVarAddress, Fix v)], [(Uuid, EntityDecl (Fix v))])

executeAPhase :: forall v g h. (ToJSON (v(Fix v)), FromJSON (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, Show (v (Fix v)), V' <<: v, WeakenF v V')
  => Free (AEff' v) () -> AIn v  ->  IO (Maybe (PageCall (BiFix h (Fix g)) (Fix g)))
executeAPhase e (heap, file, params, cache, eCache) = do
  (status, elems :: Elems V') <- openDatabase file
  let elems' = elems {entities = KM.map (fmap cmapF) $ entities elems}
  writeFile file $ encodeElems elems'
  ((((_, pageCall), log), elems'), readStatus) <- unwrap
        $ handle_ (dbWriteH file) ([] :: [WriteOps v]) 
        $ handle_ inMemoryDbReadH (elems', status)
        $ handle_ heap' (makeEnv heap)
        $ handle_ eHeapH eCache
        $ handle_ appendWriterH []
        $ handle_ redirectH Nothing
        $ handle mockThrowH -- throw (effect to remove)
        $ handle_ cacheH (Map.fromList cache)
        $ handle_ stateElH Nothing -- state address
        $ handle_ paramsH (Map.fromList params) -- reqparamsst
        $ handle_ autoIncrementState (Count 0) -- state buttincount
        $ handle_ autoIncrementState (VSeed 0) -- state tvarseed
        $ handle_ singleAccessState Nothing --state maybe labelid
        $ handle idH -- random label labelid
        $ handle_ autoIncrementState (Seed 0) -- state seed
        $ handle_ simpleStateH "" --state formid
        $ handle aH     --action
        $ e 
  (status, elemsV :: Elems v) <- openDatabase file
  let elems''' :: Elems V' = elems {entities = KM.map (fmap weakenF) $ entities elemsV}
  print "action log"
  print log
  writeFile file $ encodeElems elems'''
  return pageCall

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

instance DenoteT Action (EffV Vt) (AEff' Vt) Vt' where
  denoteT = denoteAAction

instance Lift (EffV Vt) (AEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e

instance DenoteDef' PageDef (PEnv (EffV Vt) (AEff' Vt) Vt') (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (AEff' Vt)  Vt) where
  denoteDef' = D.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (AEff' Vt) Vt) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (AEff' Vt) Vt) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV Vt) (AEff' Vt) Vt') (FreeEnv (EffV Vt) Vt')(Eff'' (EffV Vt) (AEff' Vt) Vt) where
  denoteDef'= T.denoteDefT