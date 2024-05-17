module Entity.Denotation where
import Eval.Effects
import Syntax
import Utils.Fix
import Utils.Composition
import Eval.Syntax ( Eval(Var), VName )
import Utils.Denote
import Fun.Denotation (derefDefs)
import Eval.Denotation (derefEnv)
import Entity.Syntax
import Utils.Free
import Entity.Handlers as H
import Utils.Handler
import Utils.Environment

derefH :: (Functor eff)
    => a -> Handler_ (MLState a b) b env eff (b, env) 
    -> env -> Free eff b
derefH key handler env = do
  (loc, env) <- handle_ handler env (deref key)
  return loc

denote :: forall v eff. (MLState Address (Fix v) <: eff, Null <: v, EntityEnv <: v)
  => Eval ([VName], ScopedType) (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (Var (name : names, ty) ) env = do
    loc <- derefEnv name env
    obj <- deref loc
    case projF obj of
        Just (entity :: EntityEnv (Fix v)) -> getProperties names entity

getProperty name scope = do
  loc <- derefH (name, Property) entity scope
  deref loc

getProperties [ name ] scope = getProperty name scope
getProperties (name : names) scope = do
  obj <- getProperty name scope
  case projF obj of
    Just (entity :: EntityEnv (Fix v)) -> getProperties names entity

-- denote (VDecl name k)        env = do
--     loc <- ref (injF Null :: Fix v)
--     env' <- refEnv name loc env 
--     k env'

-- denote (VValDecl name e k) env = do
--     v    <- e env
--     loc  <- ref v
--     env' <- refEnv name loc env
--     k env'

-- denote (VAssign name e)    env = do
--     v <- e env
--     (loc, _) <- derefEnv name env
--     assign (loc, v)
--     return $ injF Null