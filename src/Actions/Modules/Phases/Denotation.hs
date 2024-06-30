{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Actions.Modules.Phases.Denotation where
import Actions.Modules.Phases.Syntax (VTuple(Validate))
import Utils
import Actions.Effects
import Actions.Values as V
import Actions.Modules.Bool.Syntax
import Templates.Modules.Page.Syntax
import Templates.Syntax (LiftE, LiftT)
import Templates.Modules.Lift.Syntax

denote :: (Functor eff, LitBool <: v', v~Fix v', Null <: v'
    , Writer (TId, String) <: eff)
    => VTuple (Env eff v -> Free eff v) -> FreeEnv eff v
denote (Validate e errorMsg props) env = do
    bool <- e env
    case unbox bool of
        False -> do
            write (TId $ templateId env, errorMsg)
        True  -> return ()
    return V.null

denoteT :: (Functor eff, LitBool <: v', v~Fix v'
    , Writer (TId, String) <: eff', Lift eff eff' (Fix v'))
    => LiftE VTuple (PEnv eff eff' v) (FreeEnv  eff v) -> PEnv eff eff' v
denoteT (LiftE (Validate e errorMsg props)) env = do
    bool <- Utils.lift $ e $ actionEnv env
    case unbox bool of
        False -> do
            write (TId $ templateId $ actionEnv env, errorMsg)
        True  -> return ()