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

type Eff    = MLState Address V + Cond + End
type V      = Bool \/ Int \/ Null
type Module = Stmt + Arith + Boolean + Eval
type Out    = Maybe (Either Bool Int)

run :: FreeEnv Eff V
  -> Maybe (Either Bool Int)
run e = case unwrap
    $ handle condition
    $ flipHandle_ handle_ heap (makeEnv [])
    $ e $ Env []
  of
    (Left val, _)         -> Just $ Left val
    (Right (Left val), _) -> Just $ Right val
    (Right (Right _), _)  -> Nothing


instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote


instance Denote Eval Eff V where
  denote = E.denote

instance Denote Stmt Eff V where
  denote = S.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

--------------------------------

testStmt :: Test
testStmt = testEq "Stmt"
  Nothing
  stmtSyntax

stmtSyntax :: Fix Module
stmtSyntax = injF $ S (injF $
    VValDecl "x" (injF $ A.lit 4) Int
    (injF $ VAssign "x" (injF $ A.lit 8) Int))
    (injF $ Var "x")

stmtTests :: Test
stmtTests = TestList [
    testStmt
    ]