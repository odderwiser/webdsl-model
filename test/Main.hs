module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ActionsTest (actionsTests)
import TemplatesTest (templatesTests)
import Actions.FunTest (abortPrint)

main :: IO ()
main = do
  result <- runTestTT $ test 
    [ actionsTests
    , templatesTests
    ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


