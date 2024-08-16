{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Actions.Modules.Phases.Denotation where
import Actions.Modules.Phases.Syntax as S (VTuple(Validate), Redirect (Redirect), Ref(Ref))
import Utils
import Actions.Effects
import Actions.Values as V
import Actions.Modules.Bool.Syntax
import Templates.Modules.Page.Syntax
import Templates.Syntax (LiftE, LiftT)
import Templates.Modules.Lift.Syntax
import Templates.Effects (redirect)
import qualified Templates.Effects as E
import Syntax

renderErrorMsg (Just True)  env errorMsg = return ()
renderErrorMsg (Just False) env errorMsg = write (TId $ templateId env, errorMsg)

denote :: forall eff v' v. (Functor eff, LitBool <: v', v~Fix v', Null <: v'
    , Writer (TId, String) <: eff, Cond <: eff)
    => VTuple (Env eff v -> Free eff v) -> FreeEnv eff v
denote (Validate e errorMsg props) env = do
    bool <- e env
    renderErrorMsg (projV bool) env errorMsg
    return V.null 

denoteT :: (Functor eff, LitBool <: v', v~Fix v', Show v
    , Writer (TId, String) <: eff', Lift eff eff' (Fix v'), Writer String <: eff')
    => LiftE VTuple (PEnv eff eff' v) (FreeEnv  eff v) -> PEnv eff eff' v
denoteT (LiftE (Validate e errorMsg props)) env = do
    bool <- Utils.lift $ e $ actionEnv env
    renderErrorMsg (projV bool) (actionEnv env) errorMsg

denoteR (Redirect page args) env = return V.null

denoteA :: (E.Redirect v <: f, Null <: v', v~ Fix v')
    => Redirect (FreeEnv f v) -> FreeEnv f v
denoteA (Redirect page args) env = do
    args' <- mapM (evalArgs env id) args
    redirect page args'
    return V.null

evalArgs env lift (e, ty) = do
    e' <- lift $ e env
    return (e', ty)

denoteTR :: forall eff eff' v. (E.Redirect v <: eff', Lift eff eff' v)
    => LiftE Redirect (PEnv eff eff' v) (FreeEnv eff v) -> PEnv eff eff' v
denoteTR (LiftE (Redirect page args)) tEnv = do --todo
    args' <- mapM (evalArgs (actionEnv tEnv) Utils.lift) args
    redirect page args'


denoteRef :: (EHeap v' <: eff, v~ Fix v', Lit Uuid <: v')
    => Ref (FreeEnv eff v) -> FreeEnv eff v
denoteRef (S.Ref uuid) env = return $ box uuid