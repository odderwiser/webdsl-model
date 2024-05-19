module Entity.Handlers where
import Eval.Effects
import Entity.Syntax
import Syntax
import Utils.Handler
import Utils.Environment as U
import Fun.Syntax
import Utils.Environment (Function, Env)
import Utils.Free
import Utils.Composition
import Data.Maybe (mapMaybe)
import Entity.Effects (Write(..), write)
import Data.Foldable (find)

-- entity :: (Functor g) => Handler_ (MLState (PName, ScopedType) Address)
--   a (EntityEnv v) g (a, EntityEnv v)
-- entity = Handler_
--   { ret_ = \x map -> pure (x, map)
--   , hdlr_ = \effectCon env -> case (env, effectCon) of
--       (EEnv _ eenv, Deref key k) -> k (case lookup key eenv of Just x -> x) env
--       (_, Assign record k) -> k $ insertEnv record env
--   }
-- insertEnv record (EEnv name env) = EEnv name (record : env)

entityDefsH :: Functor eff 
  => Handler_ (MLState EName (EntityDef (FreeEnv eff v)))
  a (Env eff v) eff (a, Env eff v)
entityDefsH = mkRHandler U.entityDefs
  (\name -> find (\(EDef name' _ _) -> name == name' ))
  (\k val@(EDef name props funs) env -> k name
    $ env { U.entityDefs = val : U.entityDefs env  }
  )

refEntities :: forall eff g v eDef. (Functor eff, EntityDef <: g,
    eDef ~ EntityDef (FreeEnv eff v))
    => [g (FreeEnv eff v)] -> Env eff v
    -> Free eff (Env eff v)
refEntities entities env  = do
  (_ :: [EName], env') <- handle_ entityDefsH env 
    $ mapM ref
    $ mapMaybe (\dec -> (proj dec :: Maybe eDef)) entities
  return env'

