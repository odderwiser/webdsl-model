module PhasesFramework.Framework where
import Data.Aeson.Types (ToJSON, FromJSON)
import Utils
import Actions.Str
import Actions.Arith
import Actions.Bool
import Actions.Values
import Definitions.Program.Syntax
import qualified Templates.FrameworkIO as T
import Definitions.Pages.Framework (handleDefs, foldProgram)
import Templates.Modules.Page.Denotation (denoteP)
import Data.Maybe (fromJust)
import Definitions.Pages.Syntax (PageCall)
import Actions.FrameworkIO
import Definitions.Pages.Framework (Envs)
import Data.Bifunctor

type RenderProgram = Program ((Envs ( PEnv (EffV V') (T.Eff' V') V)) 
  (FreeEnv (EffV V') V)) (PageCall ( PEnv (EffV V') (T.Eff' V') V) (FreeEnv (EffV V') V))

runProgram :: forall v f g h . (ToJSON (v(Fix v)), FromJSON (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, Null <: v, PageCall <:: f,
  Denote h (EffV V') V, DenoteT f (EffV V') (T.Eff' V') V,
  Bifunctor f)
  => Program ((Envs (BiFix f (Fix h)))  (Fix h)) (BiFix f (Fix h))
 -> String -> IO T.Out'
runProgram f@(Fragment defs pCall) = case 
   foldProgram 
    $ Fragment defs 
    $ projPCall pCall 
  of f@(Fragment defs' pCall' ::  RenderProgram ) -> T.runApplied
      $ denoteP pCall'
      $ handleDefs f

-- projPCall :: (PageCall <:: g) => g a k -> PageCall a k
projPCall (BIn pCall) = fromJust $ proj' pCall