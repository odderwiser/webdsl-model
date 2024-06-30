module PhasesFramework.Framework where
import Data.Aeson.Types (ToJSON, FromJSON)
import Utils
import Actions.Str
import Actions.Arith
import Actions.Bool
import Actions.Values
import Definitions.Program.Syntax
import qualified Templates.FrameworkIO as T
import qualified Definitions.Pages.Framework as P
import Templates.Modules.Page.Denotation (denoteP)
import Data.Maybe (fromJust)
import Definitions.Pages.Syntax (PageCall)
import Actions.FrameworkIO
import Definitions.Pages.Framework (Envs)
import Data.Bifunctor
import PhasesFramework.Databind
import PhasesFramework.Validate
import PhasesFramework.Action (AEff', executeAPhase)
import qualified Definitions.Templates.Framework as T
-- import Templates.Modules.Page.PhasesDenotation (denotePProcess)
import PhasesFramework.Program
import qualified Definitions.GlobalVars.ActionsFramework as A
import qualified Definitions.GlobalVars.TemplatesFramework as Tp
import Syntax (Address)
import Definitions.GlobalVars.Denotation (Heap)
import Templates.Effects (State)
import Templates.Modules.Page.PhasesDenotation (denotePDb, denotePV, denotePA)

type RenderProgram e = ProgramDef (EffV V') (T.Eff' V') V e
type DbProgram e = ProgramDef (EffV Vt) (DbEff' Vt) Vt' e
type VProgram e = ProgramDef (EffV Vt) (VEff' Vt) Vt' e
type AProgram  e = ProgramDef (EffV Vt) (AEff' Vt) Vt' e

type ProgramDef eff eff' v e = Program (Def eff eff' v) e
  (PageCall ( PEnv eff eff' v) (FreeEnv eff v))

type RenderDef = Def (EffV V') (T.Eff' V') V
type DbDef  = Def (EffV Vt) (DbEff' Vt) Vt'
type VDef = Def (EffV Vt) (VEff' Vt) Vt'
type ADef = Def (EffV Vt) (AEff' Vt) Vt'
type Def eff eff' v = P.Envs ( PEnv eff eff' v) (FreeEnv eff v)
type VarDef = Def Tp.Eff Tp.Eff' V

type PgCall eff eff' v =  PageCall ( PEnv eff eff' v) (FreeEnv eff v)

runProgram :: ()
  => Program ((Envs T.Module') (Fix Module)) (Fix A.Sym) (PageCall T.Module' (Fix Module))
 -> String -> IO T.Out'
runProgram f@(Fragment defs Nothing pCall) file = P.runProgram
  (P.foldProgram (Fragment defs Nothing pCall) :: RenderProgram ()) file

runProgram f@(Fragment defs (Just vars) pCall) file = do
 (out, _) <- Tp.runObservableProgram f file
 return out

runProgram r@(Request defs (Just vars) (pCall, params)) file = do
  let (rEnv, dBEnv, vEnv, aEnv, varDef) = foldPhases defs
  let (dbCall, vCall, aCall) = foldRequest pCall
  (gVarEnv, heap, dbStatus) <- A.runVars (foldD vars) (actionEnv $ P.handleDefs varDef) [] file
  let heap' = map (second cmapF) heap
  print (gVarEnv, heap, dbStatus)
  (cache, dBvalidaton, tIds) <- executeDbPhase 
    (denotePDb dbCall $ Tp.injectGlobal (P.handleDefs dBEnv) gVarEnv) 
    heap' file params
  validation <- executeVPhase (denotePV vCall 
    $  Tp.injectGlobal (P.handleDefs vEnv) gVarEnv)  heap' file params cache tIds
  case (dBvalidaton ++ validation) of 
    [] ->  do
      nothing :: () <- executeAPhase (denotePA aCall 
        $  Tp.injectGlobal (P.handleDefs aEnv) gVarEnv)  heap' file params cache
      (T.runApplied'
        $ denoteP (foldCall pCall) $ Tp.injectGlobal (P.handleDefs rEnv) gVarEnv) heap file
    _  -> (T.runApplied'
        $ denoteP (foldCall pCall) $ Tp.injectGlobal (P.handleDefs rEnv) gVarEnv) heap file
  
  -- nothing :: () <- executeAPhase (denotePProcess aCall 
  --   $  Tp.injectGlobal (P.handleDefs aEnv) gVarEnv)  heap' file params cache

-- class Phase eff' v out where
--   execute :: Free eff' () ->  [(Address, Fix v)] -> String -> [(String, String)] -> IO out
--   denoteCall :: (PageCall (PEnv (EffV v) eff' (Fix v)) (FreeEnv (EffV v) (Fix v)) -> PEnv (EffV v) eff' (Fix v)) 
--     -> PgCall (EffV v) eff' (Fix v) -> Def (EffV v) eff' (Fix v)
--   denoteCall denoteC pCall env = denoteC pCall $ Tp.injectGlobal (P.handleDefs env) gVarEnv

  -- executePhase :: (Lift (EffV v) eff' (Fix v)
  --   , Heap v <: eff', State Address <: eff'
  --   , Null <: v') => PgCall (EffV v) eff' (Fix v) -> Def (EffV v) eff' (Fix v) 
  --  -> [(Name, Address)] -> [(Address, Fix v)] -> String -> [(String, String)] -> IO out
  -- executePhase pCall defs gVarEnv = execute (denotePProcess pCall $ Tp.injectGlobal (P.handleDefs defs) gVarEnv :: PEnv (EffV v) eff' (Fix v)) 

foldDefs:: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f)
  => [Envs (BiFix f (Fix h)) (Fix h)] -> [Envs (PEnv eff eff' v) (FreeEnv eff v)]
foldDefs = map T.foldTDefs

foldCall :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f)
  => PageCall (BiFix f (Fix h)) (Fix h) -> PageCall ( PEnv eff eff' v) (FreeEnv eff v)
foldCall = bimap foldDT foldD

foldPhases :: [P.DefSyntax] -> ([RenderDef], [DbDef], [VDef], [ADef], [VarDef])
foldPhases defs =
  ( foldDefs defs :: [RenderDef]
  , foldDefs defs :: [DbDef]
  , foldDefs defs :: [VDef]
  , foldDefs defs :: [ADef]
  , foldDefs defs :: [VarDef]
  )

foldRequest pCall =
  ( foldCall pCall :: PgCall (EffV Vt) (DbEff' Vt) Vt'
  , foldCall pCall :: PgCall (EffV Vt) (VEff' Vt) Vt'
  , foldCall pCall :: PgCall (EffV Vt) (AEff' Vt) Vt'
  )

projPCall (BIn pCall) = fromJust $ proj' pCall