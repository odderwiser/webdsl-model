module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ActionsTest (actionsTests)
import TemplatesTest (templatesTests)
import qualified Actions.GlobalVarTest as A (globalVarTests) 
import qualified Templates.GlobalVarTest as T

main :: IO ()
main = do
  globalVarA <- A.globalVarTests
  globalVarT <- T.globalVarTests
  result <- runTestTT $ test 
    [ actionsTests
    , templatesTests
    , globalVarA
    , globalVarT
    ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


