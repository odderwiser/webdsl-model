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
import Arith.Interface as A
import Bool.Interface as B
import Eval.Interface as E
import Utils.Handler
import Bool.Handlers
import Eval.Handlers
import Test.HUnit 
import Utils.Fix
import Stmt.Syntax as S
import qualified Stmt.Interface as S

type Eff = MLState Address V + Cond + End
type V =  LitB \/ LitAr \/ LitN
type Module = Stmt + Arith + Boolean + Eval

run :: (Env -> Free Eff V)
  -> Maybe (Either Bool Int)
run e = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv [])

    $ e []
  of
    (Left (B.Lit val), _)  -> Just $ Left val
    (Right (Left (A.Lit val)), _) -> Just $ Right val
    (Right (Right Null), _) -> Nothing


instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote


instance Denote Eval Eff V where
  denote = E.denote

instance Denote Stmt Eff V where
  denote = S.denote

testStmt :: Test
testStmt = TestCase (
  assertEqual "Stmt"
  Nothing
  (run $ foldD stmtSyntax
  ))

stmtSyntax :: Fix Module
stmtSyntax = injF $ S (injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ VAssign "x" (injF $ A.lit 8) Int))
    (injF $ Var "x")

stmtTests :: Test
stmtTests = TestList [
    testStmt
    ]