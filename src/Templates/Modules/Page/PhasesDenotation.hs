module Templates.Modules.Page.PhasesDenotation where
import Definitions.GlobalVars.Denotation (Heap)
import Utils
import Actions.Values
import Definitions.Templates.Syntax
import Templates.Modules.Page.Denotation (refNames)
import Templates.Modules.Forms.Syntax (EvalT(..))
import Data.Either (lefts, rights)
import Control.Monad (foldM)
import Actions.Effects (MLState, ref, assign)
import Templates.Effects (TVarAddress)
import qualified Actions.Values as V
import Syntax (Address)
import Actions.Modules.Eval.Denotation (derefEnv')

denoteBodyDb :: (Heap v' <: eff', Null <: v', Lit TVarAddress <: v'
  , Lift eff eff' v, v~Fix v', MLState TVarAddress v <: eff'
  ) => TBody (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteBodyDb (Body list) env = do
  env'  <- foldM refNames env (lefts list)
  mapM_ (refValuesDb  env') (lefts list)
  mapM_ (\e -> e env')    (rights list)

refValuesDb :: forall eff eff' v v'. 
  ( Heap v' <: eff', Lift eff eff' v
  , MLState TVarAddress v <: eff', v ~ Fix v'
  , Lit TVarAddress <: v', Null <: v')
  => TEnv eff eff' v
  -> EvalT (PEnv eff eff' v) (FreeEnv eff v)
  -> Free eff' ()
refValuesDb env (VarDeclT name)     = do
  (loc :: TVarAddress) <- ref (V.null :: v)
  (loc' :: Address) <- derefEnv' name (actionEnv env) -- I hope this is deterministic enough
  assign (loc, box loc :: v)

refValuesDb env (VarInit  name exp) = do
  loc     <- derefEnv' name (actionEnv env)
  exp'    <- lift $ exp (actionEnv env)
  (loc' :: TVarAddress) <- ref exp'
  assign
    (loc, box loc' :: v)

refValuesProcess :: forall eff eff' v v'. 
  ( Heap v' <: eff', Lift eff eff' v
  , MLState TVarAddress v <: eff', v ~ Fix v'
  , Lit TVarAddress <: v', Null <: v')
  => TEnv eff eff' v
  -> EvalT (PEnv eff eff' v) (FreeEnv eff v)
  -> Free eff' ()
refValuesProcess env (VarDeclT name)     = do
  (loc :: TVarAddress) <- ref (V.null :: v)
  (loc' :: Address) <- derefEnv' name (actionEnv env)
  assign (loc, box loc :: v)

refValuesProcess env (VarInit  name exp) = do
  loc     <- derefEnv' name (actionEnv env)
  exp'    <- lift $ exp (actionEnv env)
  (loc' :: TVarAddress) <- ref exp'
  assign
    (loc, box loc' :: v)