{-# OPTIONS_GHC -Wno-missing-fields #-}
module PhasesFramework.Databind where
import Templates.Effects
import Actions.Effects
import Utils
import Definitions.GlobalVars.Denotation (Heap)
import Actions.FrameworkIO
import Templates.FrameworkIO
import Templates.Syntax as S hiding (Databind)
import Actions.Syntax
import qualified Templates.Modules.Layout.Denotation as L
import qualified Templates.Modules.Render.Denotation as X
import qualified Templates.Modules.Page.PhasesDenotation as P
import Syntax (Address)
import qualified Templates.Modules.Lift.Denotation as Lt
import qualified Templates.Modules.Forms.PhasesDenotation as F
import Actions.Values (Lit, Uuid)
import qualified Actions.Modules.Entity.Denotation as En
import Actions.Handlers.Cond (condition)
import Actions.Handlers.Return (funReturn, dummyRedirect)
import Actions.Handlers.Entity (uuidH, eHeapH, WriteOps, mockDbReadH, dbWriteH, openDatabase, inMemoryDbReadH, Elems (entities), dbWriteInputH, encodeElems, inMemoryDbReadH', eHeapH', dbReadIoH)
import Definitions.GlobalVars.Effects (DbRead, DbWrite)
import Definitions.Templates.Syntax (TBody, TemplateDef)
import qualified Templates.Modules.Page.Denotation as F
import qualified Templates.Modules.Page.PhasesDenotation as PF
import Definitions.Pages.Syntax (PageDef, RequestParams)
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
import Templates.Handlers.Forms
import Templates.Handlers.Layout (attributeH, stateH, stateElH)
import Actions.Handlers.Heap (heap', makeEnv)
import qualified Data.Map as Map
import Templates.Handlers.Env (paramsH)
import PhasesFramework.Handlers (cacheH)
import qualified Data.Aeson.KeyMap as KM
import Templates.Modules.Phases.Denotation (denoteA)
import PhasesFramework.Validate (VEff')
import qualified Templates.Modules.Lift.Denotation as L

type DbEff' v = Databind + State FormId + State Seed
  + Random Label LabelId + State (Maybe LabelId)
  + State TVarSeed + ReqParamsSt + State Address
  + MLState TVarAddress (Fix v) + Writer (TId, String)
  + Reader () (Maybe TId) + Writer TId + State TSeed + Writer String
  + EHeap v + Heap v + DbRead (EntityDecl (Fix v)) + DbWrite (Fix v) + End

data Databind k
  deriving Functor

dH :: Functor v => Handler Databind a v a
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

instance DenoteT Action (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = denoteA

instance DenoteT Loop (EffV Vt) (DbEff' Vt) Vt' where
  denoteT = L.denoteT

instance Lift (EffV Vt) (DbEff' Vt) Vt' where
  lift e = bubbleDown
    $ handle dummyRedirect
    $ handle uuidH
    $ handle funReturn
    $ handle condition
    e

--

instance DenoteDef' PageDef (PEnv (EffV Vt) (DbEff' Vt) Vt') (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (DbEff' Vt) Vt) where
  denoteDef' = D.denoteDef

instance DenoteDef FDecl (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (DbEff' Vt) Vt) where --- Maybe?? This works???
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffV Vt) Vt') (Eff'' (EffV Vt) (DbEff' Vt) Vt) where
  denoteDef = E.denoteDef

instance DenoteDef' TemplateDef (PEnv (EffV Vt) (DbEff' Vt) Vt') (FreeEnv (EffV Vt) Vt')(Eff'' (EffV Vt) (DbEff' Vt) Vt) where
  denoteDef'= T.denoteDefT

type Cache v= [(TVarAddress, Fix v)]

type DbOut v = (Cache v, [(TId, String)], [TId], [(Uuid, EntityDecl (Fix v))])

executeDbPhase :: (ToJSON (v(Fix v)), FromJSON (v (Fix v)), Show (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, V' <<: v)
  => Free (DbEff' v) () ->  [(Address, Fix v)] -> String -> [(String, String)] 
  -> IO (DbOut v)
executeDbPhase e heap file params = do
  (status, elems :: Elems V') <- openDatabase file
  let elems' = elems {entities = KM.map (fmap cmapF) $ entities elems}
  writeFile file $ encodeElems elems'
  action <- ioHandle (dbReadIoH file) 
        $ handle_ heap' (makeEnv heap)
        $ handle_ eHeapH' []
        $ handle_ appendWriterH []
        $ handle_ autoIncrementState (TSeed 0)
        $ handle_ appendWriterH []
        $ handle_ templateIdMaybeReaderH []
        $ handle_ appendWriterH [] -- throw (effect to remove)
        $ handle_ cacheH (Map.empty)
        $ handle_ stateElH Nothing -- state address
        $ handle_ paramsH (Map.fromList params) -- reqparamsst
        $ handle_ autoIncrementState (VSeed 0) -- state tvarseed
        $ handle_ singleAccessState Nothing --state maybe labelid
        $ handle idH -- random label labelid
        $ handle_ autoIncrementState (Seed 0) -- state seed
        $ handle_ simpleStateH "" --state formid
        $ handle dH     --databind
        $ e
  (((((_, cache), validationErrors), templateIds), log), eCache) <- unwrap
        $ handle_ (dbWriteInputH file) ([] :: [WriteOps v], (elems', status)) $ action
  (status, elemsV :: Elems Vt) <- openDatabase file
  let elems''' :: Elems V' = elems {entities = KM.map (fmap weakenF) $ entities elemsV}
  writeFile file $ encodeElems elems'''
  return (cache, validationErrors, templateIds, eCache)


-- State (Maybe LabelId) 
--   + State TVarSeed + ReqParamsSt + State Address
--   + MLState TVarAddress (Fix v) + Throw 
--   + EHeap v + Heap v + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) + End