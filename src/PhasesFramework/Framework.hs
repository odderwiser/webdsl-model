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
import PhasesFramework.Databind
import PhasesFramework.Validate
import PhasesFramework.Action (AEff')
import qualified Definitions.Templates.Framework as T
import Templates.Modules.Page.PhasesDenotation (denotePProcess)
import PhasesFramework.Program

type RenderProgram = ProgramDef (EffV V') (T.Eff' V') V
type DbProgram = ProgramDef (EffV Vt) (DbEff' Vt) Vt'
type VProgram = ProgramDef (EffV Vt) (VEff' Vt) Vt'
type AProgram = ProgramDef (EffV Vt) (AEff' Vt) Vt'

type ProgramDef eff eff' v = Program (Def eff eff' v)
  (PageCall ( PEnv eff eff' v) (FreeEnv eff v))

type RenderDef = Def (EffV V') (T.Eff' V') V
type DbDef  = Def (EffV Vt) (DbEff' Vt) Vt'
type VDef = Def (EffV Vt) (VEff' Vt) Vt'
type ADef = Def (EffV Vt) (AEff' Vt) Vt'
type Def eff eff' v = Envs ( PEnv eff eff' v) (FreeEnv eff v)

type PgCall eff eff' v =  PageCall ( PEnv eff eff' v) (FreeEnv eff v)

runProgram :: forall v f g h . (ToJSON (v(Fix v)), FromJSON (v (Fix v)),
  LitStr <: v, LitInt <: v, LitBool <: v, [] <: v, Null <: v, PageCall <:: f,
  Denote h (EffV V') V, DenoteT f (EffV V') (T.Eff' V') V,
  Bifunctor f)
  => Program ((Envs T.Module')  (Fix Module)) (PageCall T.Module' (Fix Module))
 -> String -> IO T.Out'
runProgram f@(Fragment defs pCall) file = case
   foldProgram f
  of f@(Fragment defs' pCall' ::  RenderProgram ) -> (T.runApplied
      $ denoteP pCall'
      $ handleDefs defs') file

runProgram r@(Request defs (pCall, params)) file = do
  let (rEnv, dBEnv, vEnv, aEnv) = foldPhases defs -- i can't <$> here, why?
  let (dbCall, vCall, aCall) = foldRequest pCall
  cache <- executeDbPhase (denotePProcess dbCall $ handleDefs dBEnv) file params
  nothing :: () <- executeVPhase (denotePProcess vCall $ handleDefs vEnv) file params
  return ""

foldDefs:: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f)
  => [Envs (BiFix f (Fix h)) (Fix h)] -> [Envs (PEnv eff eff' v) (FreeEnv eff v)]
foldDefs = map T.foldTDefs

foldCall :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f)
  => PageCall (BiFix f (Fix h)) (Fix h) -> PageCall ( PEnv eff eff' v) (FreeEnv eff v)
foldCall = bimap foldDT foldD

foldPhases :: [Envs T.Module' (Fix Module)] -> ([RenderDef], [DbDef], [VDef], [ADef])
foldPhases defs =
  ( foldDefs defs :: [RenderDef]
  , foldDefs defs :: [DbDef]
  , foldDefs defs :: [VDef]
  , foldDefs defs :: [ADef]
  )

foldRequest pCall = 
  ( foldCall pCall :: PgCall (EffV Vt) (DbEff' Vt) Vt'
  , foldCall pCall :: PgCall (EffV Vt) (VEff' Vt) Vt'
  , foldCall pCall :: PgCall (EffV Vt) (AEff' Vt) Vt'
  )

projPCall (BIn pCall) = fromJust $ proj' pCall