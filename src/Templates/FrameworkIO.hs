module Templates.FrameworkIO where

import Templates.Effects as E
import Utils
import Templates.Modules.Attributes.Syntax
import Templates.Syntax as S
import Actions.FrameworkIO
import Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Return (funReturn, funReturn', dummyRedirect)
import Actions.Handlers.Cond (condition)
import Actions.Effects (MLState, Random, EHeap, Writer)
import Syntax (Address)
import Actions.Handlers.Heap (heap, makeEnv, heap')
import Templates.Modules.Layout.Denotation as L
import Templates.Modules.Render.Denotation as X
import Templates.Modules.Page.Denotation as P
import Templates.Modules.Lift.Denotation as Lt
import Templates.Modules.Lift.Syntax (LiftT)
import Actions.Syntax (Stmt, Eval)
import Actions.Handlers.Entity (uuidH, eHeapH, WriteOps, dbWriteH, mockDbReadH, inMemoryDbReadH, openDatabase, Elems, dbReadIoH)
import Templates.Modules.Forms.Denotation as F
import Templates.Handlers.Forms (singleAccessState, idH, autoIncrementState, simpleStateH, appendWriterH, templateIdMaybeReaderH, nonConsumingReaderH)
import Actions.Modules.Entity.Syntax (Entity, EntityDecl)
import Definitions.Templates.Syntax (TBody)
import Actions.Values (Lit, Null, Uuid)
import Definitions.GlobalVars.Effects (DbWrite, DbRead)
import Actions.Str (LitStr)
import Actions.Bool (LitBool)
import Actions.Arith (LitInt)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Map as Map
import Templates.Modules.Phases.Denotation (denoteAAction, denoteA)
import Actions.Modules.Stmt.Syntax (Loop)
import qualified Templates.Modules.Lift.Denotation as St
 
type Eff' v = State ButtonCount + State FormId + State Seed + Random Label LabelId + State (Maybe LabelId) 
  + Attribute + Stream HtmlOut + State AttList + E.Render (Fix v) + State Address 
  + Reader () (Maybe TId) + Writer TId + State TSeed 
   + Reader TId [String] +  E.Render String + Writer String + EHeap v
  + MLState Address (Fix v) + DbRead (EntityDecl (Fix v)) +  DbWrite (Fix v) +   End
type T = Loop +: Input (Fix Module) +: Forms +: Layout +: S.Render +: Page +: LiftT Stmt +: TBody +: EvalT +: Action
--running syntax
type Module' = BiFix T (Fix Module)   
type Out' = String


run :: (ToJSON (v(Fix v)), FromJSON (v (Fix v)), Show (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => PEnv (EffV v) (Eff' v) (Fix v) -> String
  -> IO  Out'
run e = runEnv e (TEnv { actionEnv = Env {}})

runEnv :: (ToJSON (v(Fix v)), FromJSON (v (Fix v)), 
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, Show (v (Fix v))) 
  => PEnv (EffV v) (Eff' v) (Fix v) -> TEnv (EffV v) (Eff' v) (Fix v) -> String
  -> IO Out'
runEnv e env = runApplied (e env) 

runApplied :: (ToJSON (v(Fix v)), FromJSON (v (Fix v)), Show (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => Free (Eff' v) () -> String -> IO Out'
runApplied e file = runApplied' e [] file


runApplied' :: (ToJSON (v(Fix v)), FromJSON (v (Fix v)), Show (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => Free (Eff' v) () -> [(Address, (Fix v))] -> String -> IO Out'
runApplied' e heap file = do
  (status, elems :: Elems v) <- openDatabase file
  action <-  ioHandle (dbReadIoH file)
        $ handle_ heap' (makeEnv heap)
        $ handle_ eHeapH []
        $ handle_ appendWriterH []
        $ handle renderErrorH 
        $ handle_ nonConsumingReaderH Map.empty
        $ handle_ autoIncrementState (TSeed 0)
        $ handle_ appendWriterH []
        $ handle_ templateIdMaybeReaderH []
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
        $ e 
  ((((_, out), templateIds), log), readstatus) <- unwrap
        $ handle_ (dbWriteH file) ([] :: [WriteOps v]) 
        $ action
  return out
 

instance (Lit Uuid <: v) => Lift (EffV v) (Eff' v) (Fix v) where
  lift  = handleExp



handleExp :: Free (EffV v) (Fix v) -> Free (Eff' v) (Fix v)
handleExp e = bubbleDown
  $ handle dummyRedirect
  $ handle uuidH
  $ handle funReturn
  $ handle condition
  e
    

-- probably a beeter way to implement this??
bubbleDown ::
    (eff <<: eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance DenoteT Layout (EffV v) (Eff' v) (Fix v) where
  denoteT = L.denote

instance (LitStr <: v) 
  => DenoteT S.Render (EffV v) (Eff' v) (Fix v) where
  denoteT = X.denote

instance (Null <: v, Lit Uuid <: v) 
  => DenoteT Page (EffV v) (Eff' v) (Fix v) where
  denoteT = P.denote

instance DenoteT (LiftT Stmt) (EffV v) (Eff' v) (Fix v) where
  denoteT = Lt.denote

instance ( LitStr <: v, LitBool <: v, LitInt <: v) 
 => DenoteT Forms (EffV v) (Eff' v) (Fix v) where
  denoteT = F.denoteR

instance (LitStr <: v, LitBool <: v, LitInt <: v, Null <: v, [] <: v,
  Eq (v (Fix v)), Show (v (Fix v))) 
  => DenoteT (Input (Fix Module)) (EffV v) (Eff' v) (Fix v) where
  denoteT = F.denoteRInput

instance (Null <: v, Lit Uuid <: v) 
  => DenoteT TBody (EffV v) (Eff' v) (Fix v) where
  denoteT = P.denoteBody

instance (Lit Uuid <: v) => DenoteT EvalT (EffV v) (Eff' v) (Fix v) where
  denoteT = P.denoteE

instance ( Lit Uuid <: v) => DenoteT Action (EffV v) (Eff' v) (Fix v) where
  denoteT = denoteA

instance ( Lit Uuid <: v, LitBool <: v, LitInt <: v, Null <: v, [] <: v) 
  => DenoteT Loop (EffV v) (Eff' v) (Fix v) where  
  denoteT = St.denoteT 
