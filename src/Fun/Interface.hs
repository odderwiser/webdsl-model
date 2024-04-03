module Fun.Interface where
import Fun.Effects
import Utils.Composition
import Fun.Syntax
import Utils.Denote
import Utils.Free
import Eval.Effects
import Syntax
import Eval.Interface (refEnv)
import Eval.Handlers (environment)
import Utils.Handler (handle_)
import Fun.Handlers (defs)

refVars varNames locs = handle_ environment (mapM assign (zip varNames locs))
derefDefs name = handle_ Fun.Handlers.defs (deref name) 
refDefs :: Functor eff
    => [FDecl (FreeEnv eff v)] -> Env eff v 
    -> Free eff ([FunName], Env eff v)
refDefs decls = handle_ Fun.Handlers.defs (mapM ref decls)

-- does this still work with how Variable Declaration is defined?
denote :: (Abort v <: eff, MLState Address v <: eff, Null < v)
  => Fun (Env eff v-> Free eff v)
  -> Env eff v -> Free eff v
denote (Return e)       env = do
    e' <- e env
    abort e'

denote (FCall name vars) env = do
    (FDecl _ varNames (body :: FreeEnv eff v), _) <- derefDefs name env
    (locs :: [Address])                           <- mapM (
        \e -> do
            e' <- e env
            ref e'
        ) vars
    (_, env) <- refVars varNames locs env -- this might nor work, are the assignments chained correctyl?
    inj $ body env


denoteProgram :: (Functor eff)
  => Program (Env eff v -> Free eff v)
  -> Env eff v-> Free eff v
denoteProgram (Program decls program) env = do
    (_, env') <- refDefs decls env
    program env'

    --- record or lens
