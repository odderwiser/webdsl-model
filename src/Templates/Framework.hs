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
import Actions.Effects (MLState)
import Syntax (Address)
import Actions.Handlers.Heap (heap, makeEnv)
import Templates.Modules.Layout.Denotation as L
import Templates.Modules.Render.Denotation as X
import Templates.Modules.Page.Denotation as P
import Templates.Modules.Lift.Denotation as Lt
import Control.Natural
import Templates.Modules.Lift.Syntax (LiftT)
import Actions.Syntax (Stmt)
import Actions.Handlers.Entity (uuidH)

type Eff' = Attribute + Stream HtmlOut + State AttList + E.Render V' + MLState Address V + State Address + End
type T = Layout +: S.Render +: Page +: LiftT Stmt
--running syntax
type Module' = BiFix T (Fix Module)   
type Out' = String


run :: PEnv Eff Eff' V
  -> Out'
run e = runEnv e (TEnv { actionEnv = Env {}})

runEnv :: PEnv Eff Eff' V -> TEnv Eff Eff' V
  -> Out'
runEnv e env = runApplied $ e env

runApplied :: Free Eff' () -> Out'
runApplied e = case unwrap
    $ handle_ stateElH Nothing
    $ handle_ heap (makeEnv [])
    $ handle renderH
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { R.title = Nothing, body = ""})
    $ handle_ attributeH ("section", 1)
    $ e 
  of
    ((_, str), heap)    -> str

instance Lift Eff Eff' V where
  lift  = handleExp



handleExp :: () => Free Eff V -> Free Eff' V
handleExp e = bubbleDown 
  $ handle uuidH
  $ handle funReturn
  $ handle condition
  e
    

-- probably a beeter way to implement this??
bubbleDown ::
    (eff <<: eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance DenoteT Layout Eff Eff' V where
  denoteT :: Layout (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = L.denote

instance DenoteT S.Render Eff Eff' V where
  denoteT :: S.Render (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = X.denote

instance DenoteT Page Eff Eff' V where
  denoteT = P.denote

instance DenoteT (LiftT Stmt) Eff Eff' V where
  denoteT = Lt.denote