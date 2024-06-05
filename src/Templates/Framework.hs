{-# OPTIONS_GHC -Wno-missing-fields #-}
module Templates.Framework where
import Templates.Effects as E
import Utils
import Templates.Modules.Attributes.Syntax
import Templates.Syntax as S
import Actions.Framework
import Templates.Handlers.Render as R
import Templates.Handlers.Layout
import Actions.Handlers.Return (funReturn)
import Actions.Handlers.Cond (condition)
import Actions.Effects (MLState)
import Syntax (Address)
import Actions.Handlers.Heap (heap, makeEnv)
import Templates.Modules.Layout.Denotation as L
import Templates.Modules.Render.Denotation as X

type Eff' = Attribute + Stream HtmlOut + State AttList + E.Render V' + MLState Address V + End
type T = Layout +: S.Render
--running syntax
type Module' = BiFix T (Fix Module)   
type Out' = String

run :: PEnv Eff Eff' V
  -> Out'
run e = case unwrap
    $ handle_ heap (makeEnv [])
    $ handle renderH
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { R.title = Nothing, body = ""})
    $ handle_ attributeH ("section", 1)
    $ e  (Env {}) (TEnv {}) handleExp
  of
    ((_, str), heap)    -> str


handleExp :: () => Free Eff V
  -> Free Eff' V
handleExp e = case  
    handle funReturn
    $ handle condition
    e
    of
    exp -> bubbleDown exp

-- probably a beeter way to implement this??
bubbleDown ::
    (eff ~> eff')
    => Free eff v -> Free eff' v
bubbleDown = fold Pure (Op . cmap)

instance DenoteT Layout Eff Eff' V where
  denoteT :: Layout (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = L.denote

instance DenoteT S.Render Eff Eff' V where
  denoteT :: S.Render (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = X.denote