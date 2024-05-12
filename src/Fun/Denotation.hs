module Fun.Denotation where
import Fun.Effects
import Utils.Composition
import Fun.Syntax
import Utils.Denote
import Utils.Free
import Eval.Effects
import Syntax
import Eval.Handlers (environment)
import Utils.Handler (handle_)
import Fun.Handlers (defs)
import Utils.Fix

refVars varNames locs env = handle_ environment env (mapM assign (zip varNames locs))

derefDefs :: Functor remEff 
    => FunName -> Env remEff v 
    -> Free remEff (FDecl (FreeEnv remEff v), Env remEff v)
derefDefs name env = handle_ Fun.Handlers.defs env (deref name) 

refDefs :: Functor eff
    => [FDecl (FreeEnv eff v)] -> Env eff v 
    -> Free eff ([FunName], Env eff v)
refDefs decls env  = handle_ Fun.Handlers.defs env (mapM ref decls)

-- does this still work with how Variable Declaration is defined?
denote :: (Abort (Fix v) <: eff, MLState Address (Fix v) <: eff, Null <: v)
  => Fun (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (Return e)       env = do
    e' <- e env
    abort e'

denote (FCall name vars) env = do
    (FDecl _ varNames (body :: FreeEnv eff (Fix v)), _) <- derefDefs name env
    (locs :: [Address])                           <- mapM (
        \e -> do
            e' <- e env
            ref e'
        ) vars
    (_, env) <- refVars varNames locs env -- this might nor work, are the assignments chained correctyl?
    inj $ body env


denoteProgram :: (Functor eff)
  => Program (FreeEnv eff v)
  -> FreeEnv eff v
denoteProgram (Program decls program) env = do
    (_, env') <- refDefs decls env
    program env'

    --- record or lens
