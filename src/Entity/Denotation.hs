{-# OPTIONS_GHC -Wno-missing-fields #-}
module Entity.Denotation where
import Eval.Effects
import Syntax
import Utils.Fix
import Utils.Composition
import Eval.Syntax
import Utils.Denote
import Fun.Denotation (derefDefs)
import Fun.Handlers
import Eval.Denotation (derefEnv)
import Entity.Syntax
import Utils.Free
import Utils.Handler
import Utils.Environment
import Fun.Syntax
import Program.Effects
import Data.IntMap (mapMaybe)
import Entity.Handlers as H
import qualified Program.Denotation as P
import qualified Fun.Denotation as F

derefH :: (Functor eff)
    => a -> Handler_ (MLState a b) b env eff (b, env) 
    -> env -> Free eff b
derefH key handler env = do
  (loc, env) <- handle_ handler env (deref key)
  return loc


getProperty name scope = do
  loc <- derefH (name, Property) entity scope
  deref loc

denote :: forall v eff. (MLState Address (Fix v) <: eff, Null <: v, EntityEnv <: v)
  => Eval (FreeEnv eff (Fix v), PName) (FreeEnv eff (Fix v))
  -> FreeEnv eff (Fix v)
denote (Var (object, propName) ) env = do
    obj <- object env
    case projF obj of
        Just (entity :: EntityEnv (Fix v)) -> getProperty propName entity

denote (VAssign (object, propName) e)    env = do
  obj <- object env
  case projF obj of
    Just (entity :: EntityEnv (Fix v)) -> do
      loc <- derefH (propName, Property) H.entity entity
      v   <- e env
      assign (loc, v)
      return $ injF Null

-- denote (VValDecl name e k) env = do
--     v    <- e env
--     loc  <- ref v
--     env' <- refEnv name loc env
--     k env'

denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
  => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
denoteDefs defs = P.denoteDefs Entities defs 
  $ F.denoteDefs defs


instance Def Entity where  
  foldDef :: (Def Entity, Denote f eff v) 
    => Entity (Fix f) -> Entity (FreeEnv eff v)
  foldDef (EntityDecl name props funs) = EntityDecl name props $ map foldDef funs
