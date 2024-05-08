module ColTest where
import Utils.Composition
import Col.Syntax
import Expr.Syntax
import Bool.Effects (Cond)
import Utils.Denote
import Utils.Fix
import Test.HUnit
import Utils.Handler
import Bool.Handlers
import qualified Arith.Denotation as A
import qualified Bool.Denotation as B
import qualified Expr.Denotation as E
import qualified Col.Denotation as C
import Arith.Syntax as A
import Syntax
import Bool.Syntax as B

-- type Eff    = Cond + End
-- type V      =  Recursive (Bool \/ Int)
-- type V'      = Fix (Bool + Int)
-- type Module = Arith + Boolean + Expr + Col
-- type Out    = Recursive (Either Bool Int)

-- run :: FreeEnv Eff V
--   -> Out
-- run e = case unwrap
--     $ handle condition
--     $ e $ Env {}
--   of 
--     res -> res

-- instance Denote Arith Eff V where
--   denote :: Arith (FreeEnv Eff V) -> FreeEnv Eff V
--   denote = A.denote

-- instance Denote Boolean Eff V where
--   denote :: Boolean (FreeEnv Eff V) -> FreeEnv Eff V
--   denote = B.denote

-- instance Denote Expr Eff V where
--   denote = E.denote

-- instance Denote Col Eff V where
--   denote = C.denote

-- testEq :: Denote m Eff V 
--   => String -> Out -> Fix m -> Test
-- testEq id res syntax =  TestCase $
--   assertEqual id res $ run $ foldD syntax

-- testInt = testEq
--     "contains int"
--     (Base $ Left True)
--     (injF $ Col.Syntax.In Int 
--         (injF $ A.lit 1) 
--         (injF $ LitC $ map injF 
--             [A.lit 2, 
--             A.lit 4,
--             OpArith Sub (injF $ A.lit 3) (injF $ A.lit 2)]) 
--             :: Fix Module) 

-- testBool = testEq
--     "contains bool"
--     (Base $ Left False)
--     (injF $ Col.Syntax.In Int 
--         (injF $ B.lit True) 
--         (injF $ LitC $ 
--             [injF $ B.lit False, 
--             injF $ OpB And (injF $ B.lit True) (injF $ B.lit False),
--             injF $ OpCmp Neq (injF $ A.lit 3, Int) (injF $ A.lit 3, Int)]) 
--             :: Fix Module) 

-- colTests :: Test
-- colTests = TestList 
--     [ testInt
--     , testBool
--     ]
