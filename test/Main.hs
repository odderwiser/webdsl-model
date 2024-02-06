module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ArithTest ( arithTests )
import BoolTest
import ArithBoolTest (arithBoolTests)

main :: IO ()
main = do
    result <- runTestTT $ test [arithTests, boolTests, arithBoolTests]
    print result
    if failures result > 0 then exitFailure else exitSuccess


