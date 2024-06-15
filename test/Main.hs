module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ActionsTest (actionsTests)
import TemplatesTest (templatesTests)
import Actions.GlobalVarTest (globalVarTests)

main :: IO ()
main = do
  globalVarT <- globalVarTests
  result <- runTestTT $ test 
    [ actionsTests
    , templatesTests
    , globalVarT
    ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


