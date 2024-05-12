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
    (injA 1) 
    (injC  
      [ injA  2
      , injA  4
      , injF $ OpArith Sub (injA 3) (injA 2)
      ]) :: Fix Module) 

testBool = testEq
  "contains bool"
  (injF $ B.Lit False)
  (injF $ OpIn
    (injB True) 
    (injC 
      [ injB False
      , injF $ OpB   And (injB True) (injB False)
      , injF $ OpCmp Neq (injA 3) (injA 3)
      ]) :: Fix Module) 

testList = testEq
  "contains list"
  (injF $ B.Lit True)
  (injF $ OpIn
    (injC [injA 1]) 
    (injC 
      [ injC []
      , injC [injF $ OpArith Sub (injA 3) (injA 2)]
      , injC 
        [ injF $ OpArith Add (injA 2) (injA 3)
        , injF $ OpArith Mul (injA 3) (injA 3)
        ]]) :: Fix Module)

colTests :: Test
colTests = TestList 
    [ testInt
    , testBool
    , testList
    ]
