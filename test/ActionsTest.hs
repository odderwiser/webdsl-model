module ActionsTest where
import Test.HUnit (Test (TestList))
import Actions.ArithTest (arithTests)
import Actions.ArithBoolTest (arithBoolTests)
import Actions.BoolTest (boolTests)
import Actions.StrTest (strTests)
import Actions.ColTest (colTests)
import Actions.EvalTest (evalTests)
import Actions.ExprTest (exprTests)
import Actions.FunTest (funTests)
import Actions.LoopTest (loopTests)
import Actions.StmtTest (stmtTests)
import Actions.EntityTest (entityTests)

actionsTests :: Test
actionsTests = TestList 
  [ arithTests
  , arithBoolTests
  , boolTests
  , strTests
  , colTests
  , evalTests
  , exprTests
  , funTests
  , loopTests
  , stmtTests
  , entityTests
  ]