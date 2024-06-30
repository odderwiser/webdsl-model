{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use if" #-}
module Actions.Modules.Phases.Denotation where
import Actions.Modules.Phases.Syntax (VTuple(Validate))
import Utils
import Actions.Effects
import Actions.Values as V
import Actions.Modules.Bool.Syntax
import Templates.Modules.Page.Syntax

denote :: (Functor eff, LitBool <: v', v~Fix v', Null <: v'
    , Writer (TId, String) <: eff)
    => VTuple (Env eff v -> Free eff v) -> FreeEnv eff v
denote (Validate e errorMsg) env = do
    bool <- e env
    case unbox bool of
        False -> do
            write (TId $ templateId env, errorMsg)
        True  -> return ()
    return V.null