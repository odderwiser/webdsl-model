{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.GlobalVars.ActionsFramework where


import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH, globalNamesH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH, eHeapH, uuidH, mockDbReadH, dbWriteH, Elems (..), TempEHeap, tempEHeapH, mockDbWriteH, MaybeEntity, WriteOps, DbStatus (Failure, Empty), openDatabase, inMemoryDbReadH, tempEHeapH')
import Actions.Handlers.Return (funReturn, dummyRedirect)
import Actions.FrameworkIO as A
import Syntax
import Actions.Arith as A
import Actions.Bool as B
import Actions.Syntax
import Actions.Str
import Definitions.GlobalVars.Syntax (GlobalVar, VarList (VList), Uuid)
import Definitions.GlobalVars.Denotation as G
import Definitions.GlobalVars.Effects
import Actions.Modules.Col.Denotation as C
import Actions.Modules.Entity.Denotation as En
import Actions.Modules.Eval.Denotation as Ev
import Actions.Modules.Expr.Denotation as Ex
import Actions.Modules.Fun.Denotation as F
import Actions.Modules.Stmt.Denotation as S

import Actions.Modules.Str.Denotation as Str
import qualified Actions.Modules.Stmt.Denotation as St
import Actions.Handlers.Heap
import Actions.Handlers.Cond
import Data.Aeson.KeyMap (empty)
import Actions.Effects
import Actions.Values (Lit, Null)
import Data.Aeson (ToJSON, FromJSON, Result (Success))
import qualified Definitions.Entity.Framework as E
import Actions.Modules.Phases.Syntax
import qualified Actions.Modules.Phases.Denotation as Ph
import qualified Templates.Effects as E
--running syntax

--preprocessing
type Envs = EntityDef + FDecl
type EffP v = E.Eff' (EffA v) v --EntityDefsEnv (EffA v) (Fix v) + FunctionEnv (EffA v) (Fix v) + End
type EffA v = Abort (Fix v)
  + Cond + Random String String
  +  Writer (VName, Address) + E.Redirect (Fix v)
  + TempEHeap v + EHeap v + MLState Address (Fix v)
  + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) +  End
type Sym = VarList + Module

  --  case unwrap
  -- $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env (EffV V') V )
  -- $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (EffV V') V )
  -- $ denoteDefList defs of
  --   ((_, env'), env) -> run exp Env
  --     { varEnv = []
  --     , entityDefs = entityDefs env'
  --     , defs = U.defs env
  --     } []


runProgram :: ()
  => Program (E.Envs (Fix Module))  (Fix Sym) (Fix Module)
  -> FilePath -> IO (Out, DbStatus)
runProgram (Fragment defs (Just vars) exp) file = do
  let (pDefs, vDefs)
        = ( E.handleDefs (map (fmap foldD) defs ::  [Envs (FreeEnv (EffV V') (Fix V'))])
          , E.handleDefs (map (fmap foldD) defs ::  [Envs (FreeEnv (EffA V') (Fix V'))]))
  (gVarEnv, heap, dbStatus) <- runVars (foldD vars :: FreeEnv (EffA V') (Fix V')) vDefs [] file
  print gVarEnv
  (v, status) <- run (foldD exp) (pDefs {globalVars = gVarEnv}) heap file
  return (v, dbStatus)



instance DenoteDef FDecl (FreeEnv (EffA v) (Fix v)) (EffP v) where
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffA v) (Fix v)) (EffP v) where
  denoteDef = E.denoteDef


  -- indexed family to model datatypes

-- runExp :: FreeEnv (EffA V') V -> FilePath -> IO (Out,  DbStatus)
-- runExp e = run e (Env { varEnv = []}) []

runVars :: (ToJSON (v (Fix v)), Lit Uuid <: v, FromJSON (v (Fix v)), Show (v (Fix v)))
  => FreeEnv (EffA v) (Fix v) -> Env (EffA v) (Fix v) -> [(Address, Fix v)] -> FilePath
  -> IO ([(Name, Address)], [(Address, (Fix v))], DbStatus)
runVars e env store file = do
  (status, db :: Elems v) <- openDatabase file
  let (action, elems' ) = unwrap
        $ handle_ inMemoryDbReadH (db, status)
        $ handle_ (dbWriteH file) ([] :: [WriteOps v])
        $ handle_ heap'' store
        $ handle_ eHeapH []
        $ handle_ tempEHeapH' (makeEnv [])
        $ handle dummyRedirect
        $ handle_ globalNamesH []
        $ handle uuidH
        $ handle condition
        $ handle funReturn
        $ e env
  (((_, globalEnv), heap), dbstatus) <- action
  return (globalEnv, heap, status)

instance (Lit Uuid <: v, Lit Address <: v,
 EntityDecl <: v, LitStr <: v, Null <: v,
 Show (v (Fix v)))
 =>  Denote VarList (EffA v) (Fix v) where
  denote = G.denote

instance (LitInt <: v) => Denote Arith (EffA v) (Fix v) where
  denote = A.denote

instance (LitBool <: v) => Denote Boolean (EffA v) (Fix v) where
  denote = B.denote

instance (Null <: v) => Denote Eval (EffA v) (Fix v) where
  denote = Ev.denote

instance Denote Stmt (EffA v) (Fix v) where
  denote = S.denote

instance (Eq (v (Fix v)), LitBool <: v, [] <: v, LitInt <: v, Null <: v)
  => Denote Col (EffA v) (Fix v) where
  denote = C.denote

instance ( Eq (v (Fix v)), LitInt <: v,
 LitBool <: v) =>  Denote Expr (EffA v) (Fix v) where
  denote = Ex.denote

instance (LitStr <: v) => Denote Str (EffA v) (Fix v) where
  denote = Str.denote

instance (Null <: v, [] <: v, LitBool <: v, LitInt <: v)
  => Denote Loop (EffA v) (Fix v) where
  denote = St.denoteLoop

instance (Null <: v) => Denote Fun (EffA v) (Fix v) where
  denote = F.denote

instance (Null <: v, Lit Uuid <: v)
  => Denote Entity (EffA v) (Fix v) where
  denote = En.denote

instance (Lit Address <: v, Lit Uuid <: v,
 LitStr <: v, Null <: v, Show (v (Fix v)))
 => Denote EntityDecl (EffA v) (Fix v) where
  denote = En.denoteEDecl

instance (Null <: v) => Denote Redirect (EffA v) (Fix v) where
  denote = Ph.denoteA