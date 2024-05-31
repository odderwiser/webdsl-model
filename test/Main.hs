module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ActionsTest (actionsTests)

main :: IO ()
main = do
  result <- runTestTT $ test 
    [ actionsTests
    ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


