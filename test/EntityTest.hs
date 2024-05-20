{-# LANGUAGE ScopedTypeVariables #-}
module EntityTest where
import Eval.Effects
import Syntax
import Bool.Effects
import Utils.Composition
import Bool.Syntax as B
import Arith.Syntax as A
import Expr.Syntax
import Utils.Denote
import Utils.Free
import Eval.Handlers
import Utils.Handler
import Bool.Handlers
import Bool.Denotation as B
import Arith.Denotation as A
import Expr.Denotation as Ex
import Eval.Denotation as Ev
import Fun.Denotation as F
import Stmt.Denotation as S
import Test.HUnit
import TestSyntax
import Utils.Fix
import Eval.Syntax
import Fun.Syntax as F
import Fun.Effects
import Fun.Handlers (funReturn, defsH)
import Stmt.Syntax as S
import Utils.Environment
import Program.Syntax
import Program.Denotation as P
import Program.Handlers (defsHandler)
import Program.Effects
import Entity.Syntax
import Entity.Denotation as En
import qualified Utils.Environment as U
import qualified Entity.Syntax as En
import Fun.Handlers (FunctionEnv)
import Entity.Handlers


--IR
type Eff = MLState Address V + Cond + Abort V + End
type V =  Fix (LitBool + LitInt + Null + [] 
  + LitAddress + EntityDecl)

--running syntax
type Module = Arith + Boolean + Eval 
  + Fun + Stmt + EntityDecl + Entity  
type Out = Maybe (Either Bool Int)

--preprocessing
type Envs = EntityDef + FDecl
type Eff' = EntityDefsEnv Eff V + FunctionEnv Eff V + End

runProgram (Fragment defs exp) = case unwrap
  $ handle_ defsH (Env { varEnv = [], defs =[]} :: Env Eff V )
  $ handle_ entityDefsH (Env { entityDefs =[]} :: Env Eff V ) 
  $ denoteDefList defs of
    ((_, env'), env) -> run exp Env 
      { varEnv = []
      , entityDefs = entityDefs env'
      , defs = U.defs env  
      }

runExp :: FreeEnv Eff V -> Maybe (Either Bool Int)
runExp e = run e Env { varEnv = []}

run :: FreeEnv Eff V -> Env Eff V
  -> Maybe (Either Bool Int)
run e env = case unwrap
    $ handle funReturn
    $ handle condition
    $ handle_ heap' (makeEnv [])
    $ e env
  of
    In (L (B.Lit val))     -> Just $ Left val
    In (R (L (A.Lit val))) -> Just $ Right val
    In (R (R _))           -> Nothing

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = Ex.denote

instance Denote Eval Eff V where
  denote = Ev.denote

instance Denote Fun Eff V where
  denote = F.denote

-- instance Denote Program Eff V where
--   denote = F.denoteProgram

instance Denote Stmt Eff V where
  denote = S.denote

instance Denote EntityDecl Eff V where
  denote = En.denoteEDecl

instance Denote Entity Eff V where
  denote = En.denote

instance DenoteDef FDecl Eff Eff' V where
  denoteDef = F.denoteDef

instance DenoteDef EntityDef Eff Eff' V where
  denoteDef = En.denoteDef

testEq :: Denote m Eff V
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ runExp $ foldD syntax

-- testEqProgram :: Denote m Eff V
--   => String -> Out -> Fix m -> Test
testEqProgram id res syntax =  TestCase $
  assertEqual id res $ runProgram $ foldProgram syntax

--------------------------------

testAbort' :: Test
testAbort' = testEq "two returns"
  (Just $ Right 1)
  abortSyntax

abortSyntax :: Fix Module
abortSyntax = injF
  $ S (injF (Return $ injA 1))
  $ injF (Return $ injA 2 )

testfCall' :: Test
testfCall' = testEqProgram "simple function call"
  (Just $ Right 7)
  fCallSyntax

fCallSyntax :: Program (Envs (Fix Module)) (Fix Module)
fCallSyntax = Fragment [inj $ FDecl "addThree" ["x"]
    (injF $ OpArith Add (injVar "x") (injA 3))]
  $ injF $ F.FCall "addThree" [injA 4]

testGetProperty :: Test
testGetProperty = testEqProgram "simple function call"
  (Just $ Right 1)
  propSyntax

propSyntax :: Program (Envs (Fix Module)) (Fix Module)
propSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] []]
  $ injF $ VValDecl "dummy" 
    (injF $ EDecl "dummy1" [("y", injA 1)]) 
    (injF $ PropAccess (injVar "dummy") "y")

testMethodCall = testEqProgram "simple function call"
  (Just $ Right 6)
  objFunSyntax

objFunSyntax :: Program (Envs (Fix Module)) (Fix Module)
objFunSyntax = Fragment
  [inj $ EDef "dummy1" [("x", Int), ("y", Int)] 
    [inj $ FDecl "dummy2" [ "z" ] 
      $ injF $ Return $ injF $ OpArith Add (injVar "z") (injF $ PVar "y")]]
  $ injF $ VValDecl "dummy" 
    (injF $ EDecl "dummy1" [("y", injA 1)]) 
    (injF $ En.FCall (injVar "dummy") "dummy2" [injA $ 5] )
 
testDefsStoring :: Test
testDefsStoring = TestCase $
  assertEqual "storing the def"
  ("dummy1", [("x", Int), ("y", Int)])
  (case
  handle_ entityDefsH Env{entityDefs = []} 
    $ (denoteDefList :: [Envs (FreeEnv Eff V)] -> Free Eff' [()]) (map (fmap foldD) dummy1Definition) of
      (Pure (_, env)) ->  
        case U.entityDefs env of 
        [EDef name params funs] -> (name, params))

dummy1Definition :: [Envs (Fix Module)]
dummy1Definition = [inj $ EDef "dummy1" [("x", Int), ("y", Int)] []]

entityTests :: Test
entityTests = TestList
  [ testAbort'
  , testfCall'
  , testDefsStoring
  , testGetProperty
  , testMethodCall
  ]
