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
import Utils.Environment


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
  VValDecl "x" (injF $ A.lit 4)
    (injF $ S 
      (injF $ VAssign "x" (injA 8))
      (injVar "x")
  )

stmtTests :: Test
stmtTests = TestList 
  [ testStmt
  ]
