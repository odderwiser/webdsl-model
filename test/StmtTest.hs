module StmtTest where
import Syntax
import Eval.Effects
import Utils.Composition
import Bool.Effects
import Bool.Syntax as B
import Arith.Syntax as A
import Eval.Syntax as E
import Utils.Denote
import Utils.Free
import Arith.Denotation as A
import Bool.Denotation as B
import Eval.Denotation as E
import Utils.Handler
import Bool.Handlers
import Eval.Handlers
import Test.HUnit 
import Utils.Fix
import Stmt.Syntax as S
import qualified Stmt.Denotation as S
import Col.Syntax
import qualified Col.Denotation as C
import Expr.Syntax
import Expr.Denotation as Ex


type Eff    = MLState Address V + Cond + End
type V      = Fix (LitBool + LitInt + Null + [])
type Module = Stmt + Arith + Boolean + Eval + Col + Expr
type Out    = Fix (LitBool + LitInt + Null + [])

run :: FreeEnv Eff V
  -> Out
run e = case unwrap
    $ handle condition
    $ handle_ heap (makeEnv [])
    $ e $ Env {varEnv = []}
  of
    (out, _) -> out


instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote


instance Denote Eval Eff V where
  denote = E.denote

instance Denote Stmt Eff V where
  denote = S.denote

instance Denote Col Eff V where
  denote = C.denote

instance Denote Expr Eff V where
  denote = Ex.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

--------------------------------

testStmt :: Test
testStmt = testEq "Stmt"
  (injF $ A.Lit 8)
  stmtSyntax

stmtSyntax :: Fix Module
stmtSyntax = injF $ 
  VValDecl ["x"] (injF $ A.lit 4) Int
    (injF $ S 
      (injF $ VAssign ["x"] (injA 8) Int)
      (injF $ Var ["x"])
  )

testForLoop = testEq "forLoop"
  (injF $ A.Lit 8)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 4) Int $ injF $ S
    (injF $ ForC "e1" (injC [injB True, injB True, injB False])
      (injF $ VAssign ["x"] (injF $ OpArith Add (injVar "x") (injA 2)) Int )
      [ Where (injVar "e1")]
    ) (injVar "x") :: Fix Module)

testOrderAsc = testEq "order by ascending"
  (injF $ A.Lit 48)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 1) Int $ injF $ S
    (injF $ ForC "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign ["x"] (injF $ OpArith Mul (injF 
        $ OpArith Add (injVar "x") (injVar "e1")) (injVar "e1")) Int )
      [ OrdBy (injVar "e1") True]
    ) (injVar "x") :: Fix Module)

testOrderDesc = testEq "order by descending"
  (injF $ A.Lit 45)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 1) Int $ injF $ S
    (injF $ ForC "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign ["x"] (injF $ OpArith Mul (injF 
        $ OpArith Add (injVar "x") (injVar "e1")) (injVar "e1")) Int )
      [ OrdBy (injVar "e1") False]
    ) (injVar "x") :: Fix Module)

testLimit = testEq "order by descending"
  (injF $ A.Lit 3)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 0) Int $ injF $ S
    (injF $ ForC "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign ["x"] (injF $ OpArith Add (injVar "x") (injVar "e1")) Int )
      [ Limit (injA 2)]
    ) (injVar "x") :: Fix Module)

testOffset = testEq "order by descending"
  (injF $ A.Lit 5)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 0) Int $ injF $ S
    (injF $ ForC "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign ["x"] (injF $ OpArith Add (injVar "x") (injVar "e1")) Int )
      [ Offset (injA 1)]
    ) (injVar "x") :: Fix Module)

testLimitOffset = testEq "order by descending"
  (injF $ A.Lit 1)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 0) Int $ injF $ S
    (injF $ ForC "e1" (injC [injA 2, injA 1, injA 4])
      (injF $ VAssign ["x"] (injF $ OpArith Add (injVar "x") (injVar "e1")) Int )
      [Limit (injA 2), Offset (injA 1)]
    ) (injVar "x") :: Fix Module)

testAllFilters = testEq "order by descending"
  (injF $ A.Lit 5)
  (injF $ 
  VValDecl ["x"] (injF $ A.lit 0) Int $ injF $ S
    (injF $ ForC "e1" (injC [injA 2, injA 1, injA 4, injA 3, injA 6, injA 3])
      (injF $ VAssign ["x"] (injF $ OpArith Add (injVar "x") (injVar "e1")) Int )
      [Where (injF $ OpCmp Lt (injVar "e1") (injA 4)), OrdBy (injVar "e1") False, Limit (injA 3), Offset (injA 1)]
    ) (injVar "x") :: Fix Module)
  

stmtTests :: Test
stmtTests = TestList [
    testStmt
    , testForLoop
    , testOrderAsc
    , testOrderDesc
    , testLimit
    , testOffset
    , testLimitOffset
    , testAllFilters
    ]
