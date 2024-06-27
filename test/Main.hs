module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ActionsTest (actionsTests, actionsIOTests)
import TemplatesTest (templatesTests, templatesTestsIO)
import qualified Actions.GlobalVarTest as A (globalVarTests) 
import qualified Templates.GlobalVarTest as T

main :: IO ()
main = do
  globalVarA <- A.globalVarTests
  globalVarT <- T.globalVarTests
  actionIo <- actionsIOTests
  templateIO <- templatesTestsIO
  result <- runTestTT $ test 
    [ actionsTests
    , actionIo
    , templatesTests
    , templateIO
    , globalVarA
    , globalVarT
    ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


