{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.GlobalVars.ActionsFramework where


import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH, globalNamesH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH, eHeapH, uuidH, mockDbReadH, dbWriteH, Elems (..), TempEHeap, tempEHeapH, mockDbWriteH, MaybeEntity, WriteOps, DbStatus)
import Actions.Handlers.Return (funReturn)
import Actions.FrameworkIO as A
import Syntax
import Actions.Arith as A
import Actions.Bool as B
import Actions.Syntax
import Actions.Str
import Definitions.GlobalVars.Syntax (GlobalVar, VarList, Uuid)
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
import Data.Aeson (ToJSON, FromJSON)
import Definitions.Entity.Framework (handleDefs)
--running syntax

--preprocessing
type Envs = EntityDef + FDecl
type EffP v = EntityDefsEnv (EffA v) (Fix v) + FunctionEnv (EffA v) (Fix v) + End
type EffA v = Abort (Fix v)
  + Cond + Random String String
  +  Writer (VName, Address) + EHeap v + MLState Address (Fix v)
  + DbWrite (Fix v) + DbRead (EntityDecl (Fix v)) +  End
type Sym = VarList + Module

runProgram :: DenoteDef sym e (EffP V')
  => Program (Fix Sym)  (Fix Sym) (Fix Module)
  -> FilePath -> IO ((Out, [(Address, MaybeEntity V')]), DbStatus)
runProgram (Fragment defs vars exp) file =
  let (pDefs :: Env (EffV V') V, vDefs :: Env (EffA V') V)
        = (handleDefs defs, handleDefs  defs)
      (gVarEnv, heap, dbStatus) = runVars defs pDefs [] file
  in
    run exp (vDefs {globalVars = gVarEnv}) heap file


  
  --  case unwrap
  -- $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env (EffV V') V )
  -- $ handle_ entityDefsH (Env { entityDefs =[]} :: Env (EffV V') V )
  -- $ denoteDefList defs of
  --   ((_, env'), env) -> run exp Env
  --     { varEnv = []
  --     , entityDefs = entityDefs env'
  --     , defs = U.defs env
  --     } []


instance DenoteDef FDecl (FreeEnv (EffA v) (Fix v)) (EffP v) where
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv (EffA v) (Fix v)) (EffP v) where
  denoteDef = E.denoteDef


  -- indexed family to model datatypes

-- runExp :: FreeEnv (EffA V') V -> FilePath -> IO (Out,  DbStatus)
-- runExp e = run e (Env { varEnv = []}) []

runVars :: (ToJSON (v (Fix v)), Lit Uuid <: v, FromJSON (v (Fix v))) 
  => FreeEnv (EffA v) (Fix v) -> Env (EffA v) (Fix v) -> [(Address, Fix v)] -> FilePath
  -> IO ([(Name, Address)], [(Address, (Fix v))], DbStatus)
runVars e env store file = do
  (((_, globalEnv), heap), dbstatus) <- unwrap
      $ handle mockDbReadH
      $ handle_ (dbWriteH file) ([] :: [WriteOps v])
      $ handle_ heap'' store
      $ handle_ eHeapH []
      $ handle_ globalNamesH [] 
      $ handle uuidH
      $ handle condition
      $ handle funReturn
      $ e env
  return ([], [], dbstatus)

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