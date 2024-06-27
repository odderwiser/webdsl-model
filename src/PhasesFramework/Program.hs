module PhasesFramework.Program where
import Utils
import Data.Bifunctor
import Definitions.Program.Syntax
import Definitions.Templates.Framework hiding (foldTProgram)
import Definitions.Pages.Syntax
import Definitions.GlobalVars.Syntax (VarListT (..))
import Actions.Values
import Templates.Effects (TVarAddress)
import Templates.Syntax (PropRef)
import Actions.FrameworkIO (V')

-- foldTProgram :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g)
--     => Program ((g (BiFix f (Fix h))) (Fix h)) (BiFix f (Fix h))
--     -> (Program ((g (PEnv eff eff' v)) (FreeEnv eff v)) (PEnv eff eff' v), RequestParams)
-- foldTProgram (Request defs (t, params)) = (Fragment (fmap foldTDefs defs) (foldDT t), params)
-- foldTProgram (Fragment defs t) = (Fragment (fmap foldTDefs defs) (foldDT t), [])

-- foldProgramVT :: (Denote h eff v, DenoteT f eff eff' v, Bifunctor f, Bifunctor g, VarListT <:: f, PageCall <:: f)
--     => ProgramV (Fix h) (g (BiFix f (Fix h)) (Fix h)) (BiFix f (Fix h))
--     -> (Program ((g (PEnv eff eff' v)) (FreeEnv eff v)) (PEnv eff eff' v), RequestParams)
-- foldProgramVT (WithVars vars (Fragment defs program)) = foldTProgram $ Fragment
--     defs
--     (injBf $ VList vars program)

-- foldProgramVT (WithVars vars (Program defs)) = foldProgramVT
--     (WithVars vars (Fragment defs (injBf $ PCall "root" [] [])))

-- foldProgramVT (WithVars vars (Request defs (t, params)) ) = foldTProgram $ Request
--     defs
--     (injBf $ VList vars t, params)

type Vt = Lit TVarAddress + PropRef + V'
type Vt' = Fix Vt
