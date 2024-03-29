module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ArithTest ( arithTests )
import BoolTest
import ArithBoolTest (arithBoolTests)
import ExprTest (exprTests)
import EvalTest (evalTests)
import StmtTest (stmtTests)
import FunTest (funTests)

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
        ]
    print result
    if failures result > 0 then exitFailure else exitSuccess


