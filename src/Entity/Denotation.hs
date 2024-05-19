{-# OPTIONS_GHC -Wno-missing-fields #-}
{-# LANGUAGE DataKinds #-}
module Entity.Denotation where
import Eval.Effects
import Syntax as S
import Utils.Fix
import Utils.Composition
import Eval.Syntax
import Utils.Denote
import Fun.Denotation (derefDefs)
import Fun.Handlers
import Eval.Denotation (derefEnv, refEnv)
import Entity.Syntax
import Utils.Free
import Utils.Handler
import Utils.Environment
import Fun.Syntax
import Program.Effects
import Data.IntMap (mapMaybe)
import Entity.Handlers as H
import Program.Denotation as P
import Fun.Denotation as F
import Eval.Handlers (heap'', environment, heap')
import Fun.Effects as F

getProperty name = derefH name environment


getObjEnv obj env = do 
  (EDecl _ objEnv) <- derefH (getAddress obj) heap'' (entities env)
  return objEnv 

denoteEval :: forall e v eff. 
  (e ~ FreeEnv eff (Fix v), 
    MLState Address (Fix v) <: eff, 
    Null <: v, EntityDecl <: v, AddressBox <: v)
  => Eval (e, PName) e -> e
denoteEval (Var (object, propName) ) env = do
  obj       <- object env
  objEnv    <- getObjEnv obj env
  loc       <- getProperty propName objEnv
  deref loc

-- except that is not what really happens
-- there is some flushing semantics that need to happen fist
denoteEval (VAssign (object, propName) e)    env = do
  obj              <- object env
  objEnv           <- getObjEnv obj env
  loc              <- getProperty propName objEnv
  e'               <- e env
  assign (loc, e')
  return S.null

denoteFCall :: (e ~ FreeEnv eff (Fix v), 
  MLState Address (Fix v) <: eff, 
  Null <: v, EntityDecl <: v, AddressBox <: v) 
  => Fun (e, FunName) e -> e 
denoteFCall (FCall (obj, fname) vars) env = do
  obj'                  <- obj env
  objEnv                <- getObjEnv obj' env
  FDecl _ varNames body <- derefDefs fname objEnv
  env'                  <- F.populateEnv env varNames vars
  env''                 <- liftEnv env' objEnv 
  body env'

liftEnv global obj = handle dropH $ F.lift global obj


denoteDefs :: (GlobalScope envs eff v <: eff',FDecl <: envs)
  => [envs (FreeEnv eff v)] -> Free eff' (Env eff v)
denoteDefs defs = P.denoteDefs Entities defs 
  $ F.denoteDefs defs

instance Def EntityDef where  
  foldDef :: (Def EntityDef, Denote f eff v) 
    => EntityDef (Fix f) -> EntityDef (FreeEnv eff v)
  foldDef (EDef name props funs) = EDef name props $ map foldDef funs
