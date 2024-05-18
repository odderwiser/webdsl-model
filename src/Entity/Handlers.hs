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

entity :: (Functor g) => Handler_ (MLState (PName, ScopedType) Address)
  a (EntityEnv v) g (a, EntityEnv v)
entity = Handler_
  { ret_ = \x map -> pure (x, map)
  , hdlr_ = \effectCon env -> case (env, effectCon) of
      (EEnv _ eenv, Deref key k) -> k (case lookup key eenv of Just x -> x) env
      (_, Assign record k) -> k $ insertEnv record env
  }

entityDecls :: Functor eff => Handler_ (MLState EName (Entity Address))
  a (Env eff v) eff (a, Env eff v)
entityDecls = mkRHandler U.entities lookup
  (\k val@(EntityDecl name props funs) env -> k name
        $ env { U.entities = (name, val) : U.entities env  }
  )

insertEnv record (EEnv name env) = EEnv name (record : env)

eDefsHandler :: (Functor eff)
  => Handler_ (MLState Address (Function eff v))
  a (Env eff v) eff (a, Env eff v)
eDefsHandler = mkRHandler U.eDefs lookup
  (\ k val@(FDecl name vars body) env ->
    let address = length $ U.eDefs env in
      k address
      $ env { U.eDefs = (address, val) : U.eDefs env })

entityWriter :: forall eff remEff v a.
  (Functor eff, Functor remEff)
  => Handler_ (Write (Entity (FreeEnv eff v)))
    a (Env eff v) remEff (Env eff v)
entityWriter = Handler_ {
  ret_ = \x env -> pure env
  , hdlr_ = \(Write entity k) env ->
    case refEntity (entity :: Entity (FreeEnv eff v)) env of
      (Pure env' :: Free eff (Env eff v)) -> k env
}

writeEntities :: forall eff g v. (Functor eff, Entity <: g)
    => [g (FreeEnv eff v)] -> Env eff v
    -> Free eff (Env eff v)
writeEntities entities env  = do
  handle_ entityWriter env $ mapM write
    $ mapMaybe (\dec ->
      (proj dec :: Maybe (Entity (FreeEnv eff v)))) 
    entities

test decls = mapM write
    $ mapMaybe (\dec ->
        case proj dec of
          Just (dec' :: Entity (FreeEnv eff v)) -> Just dec'
          Nothing -> Nothing) decls

refEDefs :: (Functor eff)
    => [FDecl (FreeEnv eff v)] -> Env eff v
    -> Free eff ([Address], Env eff v)
refEDefs decls env = handle_ eDefsHandler env
    $ mapM ref decls

refEntity :: ( Functor eff)
    => Entity (FreeEnv eff v) -> Env eff v
    -> Free eff (Env eff v)
refEntity (EntityDecl name props funs) env = do
  (addresses :: [Address], env') <- refEDefs funs env
  (name :: EName, env'')         <- handle_ entityDecls env' $ ref $
    EntityDecl name props
    $ zipWith (curry (\(FDecl n v _, address) ->
      FDecl n v address)) funs addresses
  return env''
