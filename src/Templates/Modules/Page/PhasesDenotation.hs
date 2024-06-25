module Templates.Modules.Page.PhasesDenotation where
import Definitions.GlobalVars.Denotation (Heap)
import Utils
import Actions.Values
import Definitions.Templates.Syntax
import Templates.Modules.Page.Denotation (refNames)
import Templates.Modules.Forms.Syntax (EvalT(..))
import Data.Either (lefts, rights)
import Control.Monad (foldM)
import Actions.Effects (MLState, ref, assign, Random)
import Templates.Effects (TVarAddress (Address), encode, State, TVarSeed (VSeed), get)
import qualified Actions.Values as V
import Syntax (Address)
import Actions.Modules.Eval.Denotation (derefEnv')

denoteBodyDb :: (Heap v' <: eff', Null <: v', Lit TVarAddress <: v'
  , Lift eff eff' v, v~Fix v', MLState TVarAddress v <: eff'
  , State TVarSeed <: eff',Random String String <: eff'
  ) => TBody (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteBodyDb (Body list) env = do
  env'  <- foldM refNames env (lefts list)
  mapM_ (refValuesDb  env') (lefts list)
  mapM_ (\e -> e env')    (rights list)

refValuesDb :: forall eff eff' v v'. 
  ( Heap v' <: eff', Lift eff eff' v, State TVarSeed <: eff'
  , MLState TVarAddress v <: eff', v ~ Fix v'
  , Lit TVarAddress <: v', Null <: v', Random String String <: eff')
  => TEnv eff eff' v
  -> EvalT (PEnv eff eff' v) (FreeEnv eff v)
  -> Free eff' ()
refValuesDb env (VarDeclT name)     = do
  seed :: TVarSeed  <- get
  (loc :: String)   <- encode $ name ++show seed
  assign (Address loc, V.null :: v)
  (loc' :: Address) <- derefEnv' name (actionEnv env) -- I hope this is deterministic enough
  assign (loc', box $ Address loc :: v)

refValuesDb env (VarInit  name exp) = do
  loc     <- derefEnv' name (actionEnv env)
  exp'    <- lift $ exp (actionEnv env)
  seed :: TVarSeed  <- get
  (loc' :: String)   <- encode $ name ++show seed
  assign (Address loc', V.null :: v)
  assign
    (loc, box $ Address loc' :: v)

denoteBodyProcess :: (Heap v' <: eff', Null <: v', Lit TVarAddress <: v'
  , Functor eff, v~Fix v', MLState TVarAddress v <: eff'
  , State TVarSeed <: eff',Random String String <: eff'
  ) => TBody (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteBodyProcess (Body list) env = do
  env'  <- foldM refNames env (lefts list)
  mapM_ (refValuesProcess env') (lefts list)
  mapM_ (\e -> e env')    (rights list)

refValuesProcess :: forall eff eff' v v'. 
  ( Heap v' <: eff', State TVarSeed <: eff'
  , Random String String <: eff'
  , MLState TVarAddress v <: eff', v ~ Fix v'
  , Lit TVarAddress <: v', Null <: v', Functor eff)
  => TEnv eff eff' v
  -> EvalT (PEnv eff eff' v) (FreeEnv eff v)
  -> Free eff' ()
refValuesProcess env (VarDeclT name)     = do
  seed :: TVarSeed  <- get
  (loc :: String)   <- encode $ name ++show seed
  (loc' :: Address) <- derefEnv' name (actionEnv env)
  assign (loc', box $ Address loc :: v)

refValuesProcess env (VarInit  name exp) = do
  seed :: TVarSeed  <- get
  (loc :: String)   <- encode $ name ++show seed
  (loc' :: Address) <- derefEnv' name (actionEnv env)
  assign (loc', box $ Address loc :: v)