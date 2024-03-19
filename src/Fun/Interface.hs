module Fun.Interface where
import Fun.Effects
import Utils.Composition
import Fun.Syntax
import Utils.Denote
import Utils.Free
import Eval.Effects
import Syntax

-- does this still work with how Variable Declaration is defined?
denote :: (Abort v <: eff,
    MLState Address v <: eff,
    MLState FunName (FDecl (Env -> Free eff v)) <: eff)
  => Fun (Env -> Free eff v)
  -> Env -> Free eff v
denote (Return e)       env = do
    e' <- e env
    abort e'

denote (FCall name vars) env = do
    (FDecl _ varNames body) <- deref name
    locs                    <- mapM (
        \e -> do
            e' <- e env
            ref e'
        ) vars
    body $ zip varNames locs ++ env


denoteProgram :: forall eff v. (MLState FunName (FDecl (Env -> Free eff v)) <: eff)
  => Program (Env -> Free eff v)
  -> Env -> Free eff v
denoteProgram (Program decls program) env = do
    mapM_ (ref :: FDecl (Env -> Free eff v) -> Free eff FunName) decls
    program env
