module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ArithTest ( arithTests )
import BoolTest ( boolTests )
import ArithBoolTest (arithBoolTests)
import ExprTest (exprTests)
import EvalTest (evalTests)
import StmtTest (stmtTests)
import FunTest (funTests)
import ColTest (colTests)
import EntityTest (entityTests)

main :: IO ()
main = do
  result <- runTestTT $ test 
    [ arithTests
    , boolTests
    , arithBoolTests
    , exprTests
    , evalTests
    , funTests
    , stmtTests
    , colTests
    , entityTests
    ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


