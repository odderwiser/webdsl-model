{-# OPTIONS_GHC -Wno-missing-fields #-}
module Definitions.GlobalVars.ActionsFramework where


import Utils as U
import Definitions.Entity.Syntax
import Definitions.Entity.Denotation as E
import Definitions.Fun.Denotation as F
import Definitions.Fun.Syntax
import Actions.Handlers.Env (FunctionEnv, defsH)
import Definitions.Program.Syntax
import Actions.Handlers.Entity (entityDefsH, eHeapH, uuidH, mockDbReadH, dbWriteH, Elems (..), TempEHeap, tempEHeapH, mockDbWriteH, MaybeEntity)
import Actions.Handlers.Return (funReturn)
import Actions.Framework as A hiding (run)
import Syntax
import Actions.Arith as A
import Actions.Bool as B
import Actions.Syntax
import Actions.Str
import Definitions.GlobalVars.Syntax (GlobalVar, VarList)
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
--running syntax

--preprocessing
type Envs = EntityDef + FDecl
type EffP = EntityDefsEnv EffA V + FunctionEnv EffA V + End
type EffA = Abort V +  DbRead (EntityDecl V) 
    + Cond + Random String String + TempEHeap V' + EHeap V' + MLState Address V +  DbWrite (EntityDecl V) +  End 
type Sym = VarList + Module

runProgram :: DenoteDef   sym e EffP 
  => Program (sym e) (FreeEnv EffA V) 
  -> FilePath -> IO (Out, [(Address, MaybeEntity V')])
runProgram (Fragment defs exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env EffA V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env EffA V ) 
  $ denoteDefList defs of
    ((_, env'), env) -> run exp Env 
      { varEnv = []
      , entityDefs = entityDefs env'
      , defs = U.defs env  
      } []


instance DenoteDef FDecl (FreeEnv EffA V) EffP where
  denoteDef :: FDecl (FreeEnv EffA V) -> Free EffP ()
  denoteDef = F.denoteDef

instance DenoteDef EntityDef (FreeEnv EffA V) EffP where
  denoteDef :: EntityDef (FreeEnv EffA V) -> Free EffP ()
  denoteDef = E.denoteDef


  -- indexed family to model datatypes
  
runExp :: FreeEnv EffA V -> FilePath -> IO (Out, [(Address, MaybeEntity V')])
runExp e = run e (Env { varEnv = []}) []

run :: FreeEnv EffA V -> Env EffA V -> [(Address, V)] -> FilePath
  -> IO (Out, [(Address, MaybeEntity V')])
run e env store file = unwrap
    $ handle_ (dbWriteH file) (Elems {vars = empty, entities = empty, classes = empty} :: Elems V') 
    $ handle_ heap' (makeEnv store)
    $ handle_ eHeapH []
    $ handle_ tempEHeapH []
    $ handle uuidH
    $ handle condition
    $ handle mockDbReadH
    $ handle funReturn
    $ e env

instance  Denote VarList EffA V where 
  denote :: VarList (FreeEnv EffA V) -> FreeEnv EffA V
  denote = G.denote

instance Denote Arith EffA V where
  denote = A.denote

instance Denote Boolean EffA V where
  denote = B.denote

instance Denote Eval EffA V where
  denote = Ev.denote

instance Denote Stmt EffA V where
  denote = S.denote

instance Denote Col EffA V where
  denote = C.denote

instance Denote Expr EffA V where
  denote = Ex.denote

instance Denote Str EffA V where
  denote = Str.denote

instance Denote Loop EffA V where
  denote = St.denoteLoop

instance Denote Fun EffA V where
  denote = F.denote

instance Denote Entity EffA V where
  denote = En.denote

instance Denote EntityDecl EffA V where
  denote = En.denoteEDecl