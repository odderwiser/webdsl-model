module ColTest where
import Utils.Composition
import Expr.Syntax
import Bool.Effects (Cond)
import Utils.Denote
import Utils.Fix
import Test.HUnit
import Utils.Handler
import Bool.Handlers
import Arith.Denotation as A
import Bool.Denotation as B
import Expr.Denotation as E
import Col.Denotation as C
import Arith.Syntax as A
import Syntax
import Bool.Syntax as B
import Col.Syntax as C

type Eff    = Cond + End
type V      =  Fix (LitBool + LitInt + [])
type Module = Arith + Boolean + Expr + Col
type Out    =Fix (LitBool + LitInt + [])

run :: FreeEnv Eff V
  -> Out
run e = case unwrap
    $ handle condition
    $ e $ Env {}
  of 
    res -> res

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = E.denote

instance Denote Col Eff V where
  denote = C.denote

testEq :: Denote m Eff V 
  => String -> Out -> Fix m -> Test
testEq id res syntax =  TestCase $
  assertEqual id res $ run $ foldD syntax

testInt = testEq
    "contains int"
    (injF $ B.Lit True)
    (injF $ OpIn 
        (injF $ A.lit 1) 
        (injF $ LitC $ map injF 
            [A.lit 2, 
            A.lit 4,
            OpArith Sub (injF $ A.lit 3) (injF $ A.lit 2)]) 
            :: Fix Module) 

testBool = testEq
    "contains bool"
    (injF $ B.Lit False)
    (injF $ OpIn
        (injF $ B.lit True) 
        (injF $ LitC $ 
            [injF $ B.lit False, 
            injF $ OpB And (injF $ B.lit True) (injF $ B.lit False),
            injF $ OpCmp Neq (injF $ A.lit 3, Int) (injF $ A.lit 3, Int)]) 
            :: Fix Module) 

testList = testEq
    "contains list"
    (injF $ B.Lit True)
    (injF $ OpIn
        (injF $ C.lit [injF $ A.lit 1]) 
        (injF $ C.lit 
            [ injF $ C.lit [], 
              injF $ C.lit $ [injF $ A.lit 1], 
              injF $ C.lit $ [  
            injF $ OpArith Add (injF $ A.lit 2) (injF $ A.lit 3),
            injF $ OpArith Mul (injF $ A.lit 3) (injF $ A.lit 3)]]) 
            :: Fix Module)

colTests :: Test
colTests = TestList 
    [ testInt
    , testBool
    , testList
    ]
