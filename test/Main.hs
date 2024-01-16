module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ArithTest ( arithTests )

main :: IO ()
main = do
    result <- runTestTT arithTests
    print result
    if failures result > 0 then exitFailure else exitSuccess


