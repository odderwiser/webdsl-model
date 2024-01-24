module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ArithTest ( arithTests )
import BoolTest

main :: IO ()
main = do
    result <- runTestTT $ test [arithTests, boolTests]
    print result
    if failures result > 0 then exitFailure else exitSuccess


