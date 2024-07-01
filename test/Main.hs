module Main where

import System.Exit (exitFailure, exitSuccess)
import Test.HUnit
import ActionsTest (actionsTests, actionsIOTests)
import TemplatesTest (templatesTests, templatesTestsIO)
import qualified Actions.GlobalVarTest as A (globalVarTests) 
import qualified Templates.GlobalVarTest as T
import Templates.PhasesTest (phasesTests)

main :: IO ()
main = do
  actionIo <- actionsIOTests
  globalVarA <- A.globalVarTests
  globalVarT <- T.globalVarTests
  templateIO <- templatesTestsIO
  phases' <- phasesTests
  result <- runTestTT $ test [phases']
    -- [ actionsTests
    -- , actionIo
    -- , templatesTests
    -- , templateIO
    -- , globalVarA
    -- , globalVarT
    -- ]
  print result
  if failures result > 0 then exitFailure else exitSuccess


