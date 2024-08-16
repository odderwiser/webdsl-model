module Templates.Modules.Page.PhasesDenotation where
import Definitions.GlobalVars.Denotation (Heap)
import Utils
import Actions.Values
import Definitions.Templates.Syntax
import Templates.Modules.Page.Denotation (populateTCall, refElements, derefElements, derefPDef, storeTemplateId, readInTemplateId)
import Templates.Modules.Forms.Syntax (EvalT(..))
import Data.Either (lefts, rights)
import Control.Monad (foldM)
import Actions.Effects (MLState, ref, assign, Random, Writer)
import Templates.Effects (TVarAddress (Address), encode, State, TVarSeed (VSeed), get, put, Reader, Seed, TSeed)
import qualified Actions.Values as V
import Syntax (Address)
import Actions.Modules.Eval.Denotation (derefEnv')
import Templates.Modules.Page.Syntax (Page (..), TId)
import Actions.Handlers.Env (refH)
import Definitions.Pages.Syntax
import Actions.Modules.Fun.Denotation (populateEnv)

denoteEDb :: forall eff eff' v v'.
  ( Heap v' <: eff', Lift eff eff' v, State TVarSeed <: eff'
  , MLState TVarAddress v <: eff', v ~ Fix v'
  , Lit TVarAddress <: v', Null <: v', Random String String <: eff')
  => EvalT (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteEDb (VarDeclT name) env    = do
  seed :: TVarSeed  <- get
  (loc :: String)   <- encode $ name ++show seed
  assign (Address loc, V.null :: v)
  (loc' :: Address) <- derefEnv' name (actionEnv env) -- I hope this is deterministic enough
  assign (loc', box $ Address loc :: v)

denoteEDb (VarInit  name exp) env = do
  loc     <- derefEnv' name (actionEnv env)
  exp'    <- lift $ exp (actionEnv env)
  seed :: TVarSeed  <- get
  (loc' :: String)   <- encode $ name ++show seed
  assign (Address loc', exp')
  assign
    (loc, box $ Address loc' :: v)


denoteEProcess :: forall eff eff' v v'.
  ( Heap v' <: eff', State TVarSeed <: eff'
  , Random String String <: eff'
  , MLState TVarAddress v <: eff', v ~ Fix v'
  , Lit TVarAddress <: v', Null <: v', Functor eff)
  => EvalT (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteEProcess (VarDeclT name)  env     = do
  seed :: TVarSeed  <- get
  (loc :: String)   <- encode $ name ++show seed
  (loc' :: Address) <- derefEnv' name (actionEnv env)
  assign (loc', box $ Address loc :: v)

denoteEProcess (VarInit  name exp) env  = do
  seed :: TVarSeed  <- get
  (loc :: String)   <- encode $ name ++show seed
  (loc' :: Address) <- derefEnv' name (actionEnv env)
  assign (loc', box $ Address loc :: v)


denoteProcess ::forall eff eff' v v'.
  ( Heap v' <: eff', Heap v' <: eff, Null <: v'
  , Lift eff eff' v, v~Fix v'
  , State Address <: eff'
  ) => Page (PEnv eff eff' v) (FreeEnv eff v)
    -> PEnv eff eff' v
-- this should do something with vars. DOesnt yet.
denoteProcess (PNavigate {}) pEnv = return ()

denoteProcess (TCall name args Nothing) env = do
  (body, env') <- populateTCall name args env
  body env { actionEnv = env'}

denoteProcess (TCall name args (Just elems)) env = do
  (loc, env')   <- refElements env elems
  (body, env'') <- populateTCall name args env
  put loc
  body env' { actionEnv = env''}

denoteProcess Elements env = do
  loc <- get
  (env', elems) <- derefElements env loc
  elems env'

denoteDb (TCall name args Nothing) env = do
  (body, env') <- populateTCall name args env
  env'' <- storeTemplateId name env'
  body env { actionEnv = env''}

denoteDb (TCall name args (Just elems)) env = do
  (loc, env')   <- refElements env elems
  (body, env'') <- populateTCall name args env
  put loc
  env''' <- storeTemplateId  name env''
  body $ env { actionEnv = env'''}

denoteDb syntax env = denoteProcess syntax env

denoteV (TCall name args Nothing) env = do
  (body, env') <- populateTCall name args env
  env'' <- readInTemplateId env'
  body env { actionEnv = env''}

denoteV (TCall name args (Just elems)) env = do
  (loc, env')   <- refElements env elems
  (body, env'') <- populateTCall name args env
  put loc
  env''' <- readInTemplateId env''
  body $ env { actionEnv = env'''}

denoteV syntax env = denoteProcess syntax env

denotePDb :: ( MLState Address v <: eff, Lift eff eff' v
  , MLState Address v <: eff', State Address <: eff'
  , Reader () (Maybe TId) <: eff', Random String String <: eff',  Writer TId <: eff'
  , State TSeed <: eff'
  , v ~ Fix v', Null <: v') =>
    PageCall (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denotePDb (PCall name args) env = do
  (PDef name params body) :: PageDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefPDef name env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  env'' <- storeTemplateId name env'
  body env { actionEnv = env''}

denotePV :: ( MLState Address v <: eff, Lift eff eff' v
  , MLState Address v <: eff', State Address <: eff'
  , Reader () TId <: eff'
  , v ~ Fix v', Null <: v') =>
    PageCall (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denotePV (PCall name args) env = do
  (PDef name params body) :: PageDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefPDef name env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  env'' <- readInTemplateId env'
  body $ env {actionEnv = env''}

denotePA :: ( MLState Address v <: eff, Lift eff eff' v
  , MLState Address v <: eff', State Address <: eff'
  , v ~ Fix v', Null <: v') =>
    PageCall (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denotePA (PCall name args) env = do
  (PDef name params body) :: PageDef (PEnv eff eff' v) (FreeEnv eff v)
    <- derefPDef name env
  env' <- populateEnv lift (actionEnv env) (map fst params) (map fst args)
  body env {actionEnv = env'}