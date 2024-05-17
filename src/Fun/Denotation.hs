

module Fun.Denotation where
import Fun.Effects
import Utils.Composition
import Fun.Syntax
import Utils.Free
import Eval.Effects
import Syntax
import Eval.Handlers (environment)
import Utils.Handler (handle_)
import Fun.Handlers (defs)
import Utils.Fix
import Utils.Environment (Env, FreeEnv, Function)
import Data.Maybe (mapMaybe)
import Program.Syntax
import Program.Effects
import Utils.Denote

refVars varNames locs env = handle_ environment env (mapM assign (zip varNames locs))

derefDefs :: Functor remEff
    => FunName -> Env remEff v
    -> Free remEff (FDecl (FreeEnv remEff v), Env remEff v)
derefDefs name env = handle_ Fun.Handlers.defs env (deref name)

refDefs :: (Functor eff, FDecl <: g)
    => [g (FreeEnv eff v)] -> Env eff v
    -> Free eff (Env eff v)
refDefs decls env  = do
  (names :: [FunName], env') <- handle_ Fun.Handlers.defs env $ mapM ref
    $ mapMaybe (\dec ->
        case proj dec of
          Just (dec' :: FDecl (FreeEnv eff v)) -> Just dec'
          Nothing -> Nothing) decls
  return env'

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


-- denoteProgram :: forall g eff eff' v. (Functor eff, eff ~ GlobalScope FDecl eff' + eff')
--   => Program (FDecl (FreeEnv eff' v)) (FreeEnv eff' v)
--   -> FreeEnv eff' v
denoteDefs ::
  (FDecl <: g, GlobalScope g eff v <: eff')
  => [g (FreeEnv eff v)] -> Free eff' (Env eff v) -> Free eff' (Env eff v)
denoteDefs defs env = do 
  env' <- env 
  write Defs defs env'

instance Def FDecl where
  -- foldDef :: (Functor eff, Denote f eff v, FDecl <: g)
  --   => FDecl (Fix f)
  --   -> g (FreeEnv eff v)
    foldDef :: (Def FDecl, Denote f eff v) => FDecl (Fix f) -> FDecl (FreeEnv eff v)
    foldDef decl@(FDecl name vars body) = FDecl name vars $ foldD body