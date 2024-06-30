{-# OPTIONS_GHC -Wno-missing-fields #-}
module Templates.Framework where
import Templates.Effects as E
import Utils
import Templates.Modules.Attributes.Syntax
import Templates.Syntax as S
import Actions.Framework
import Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Return (funReturn, funReturn')
import Actions.Handlers.Cond (condition)
import Actions.Effects (MLState, Random, Writer)
import Syntax (Address)
import Actions.Handlers.Heap (heap, makeEnv)
import Templates.Modules.Layout.Denotation as L
import Templates.Modules.Render.Denotation as X
import Templates.Modules.Page.Denotation as P
import Templates.Modules.Lift.Denotation as Lt
import Templates.Modules.Lift.Syntax (LiftT)
import Actions.Syntax (Stmt, Eval)
import Actions.Handlers.Entity (uuidH, eHeapH)
import Templates.Modules.Forms.Denotation as F
import Templates.Handlers.Forms (singleAccessState, idH, autoIncrementState, simpleStateH, templateIdMaybeReaderH, appendWriterH)
import Actions.Modules.Entity.Syntax (Entity)
import Definitions.Templates.Syntax (TBody)
import Actions.Values (Lit, Null)
import Definitions.GlobalVars.Syntax (Uuid)
import Actions.Str (LitStr)
import Actions.Arith (LitInt)
import Actions.Modules.Bool.Syntax (LitBool)
 
type Eff' v = State ButtonCount + State FormId + State Seed + State TSeed
  + Random Label LabelId + State (Maybe LabelId) 
  + Attribute + Stream HtmlOut + State AttList 
  + Reader () (Maybe TId) + Writer TId
  + E.Render v + MLState Address (Fix v) + State Address + End
type T = Input (Fix Module) +: Forms +: Layout +: S.Render +: Page +: LiftT Stmt +: TBody +: EvalT
--running syntax
type Module' = BiFix T (Fix Module)   
type Out' = String


run :: (LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => PEnv (EffV v) (Eff' v) (Fix v)
  -> Out'
run e = runEnv e (TEnv { actionEnv = Env {}})

runEnv ::(LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => PEnv (EffV v) (Eff' v) (Fix v) -> TEnv (EffV v) (Eff' v) (Fix v)
  -> Out'
runEnv e env = runApplied $ e env

runApplied :: (LitStr <: v, LitInt <: v, LitBool <: v, [] <: v) 
  => Free (Eff' v) () -> Out'
runApplied e = case unwrap
    $ handle_ stateElH Nothing             -- state address 
    $ handle_ heap (makeEnv [])            -- heap v
    $ handle renderH                       -- render v
    $ handle_ appendWriterH [] -- writer
    $ handle_ templateIdMaybeReaderH [] -- reader 
    $ handle_ stateH []                    -- state attlist
    $ handle_ renderHtmlH                  -- stream htmlout
      (PageR { R.title = Nothing, body = "", pageCall = False})
    $ handle_ attributeH ("section", 1)    -- attribute
    $ handle_ singleAccessState Nothing    -- state maybe labelid
    $ handle idH                           -- random label labelid
    $ handle_ autoIncrementState (TSeed 0)  -- state seed
    $ handle_ autoIncrementState (Seed 0)  -- state seed
    $ handle_ simpleStateH ""              -- state formid
    $ handle_ autoIncrementState (Count 0) -- state buttincount
    $ e 
  of
    (((_, str), templateIds), heap)    -> str

instance (Lit Uuid <: v) => Lift (EffV v) (Eff' v) (Fix v) where
  lift  = handleExp



handleExp :: Free (EffV v) (Fix v) -> Free (Eff' v) (Fix v)
handleExp e = bubbleDown
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
 =>  DenoteT Forms (EffV v) (Eff' v) (Fix v) where
  denoteT = F.denoteR

instance (LitStr <: v, LitBool <: v, LitInt <: v, Null <: v, [] <: v,
  Eq (v (Fix v))) 
  => DenoteT (Input (Fix Module)) (EffV v) (Eff' v) (Fix v) where
  denoteT = F.denoteRInput

instance (Null <: v, Lit Uuid <: v) 
  => DenoteT TBody (EffV v) (Eff' v) (Fix v) where
  denoteT = P.denoteBody

instance (Lit Uuid <: v) => DenoteT EvalT (EffV v) (Eff' v) (Fix v) where
  denoteT = P.denoteE