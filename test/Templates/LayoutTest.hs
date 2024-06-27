{-# LANGUAGE EmptyCase #-}
module Templates.LayoutTest where
import qualified Test.HUnit as T
import Utils
import Actions.Framework   as Ac
import Templates.Framework   as Tp
import Templates.Syntax as S

testEq :: ()
  => String -> Out' -> Module' -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ Tp.run (foldDT syntax :: PEnv (EffV V') (Eff' V') V)

testSyntax = testEq
    "test if templates work"
    (   "<html><head></head><body>"
     ++ "<span class=\"section section1\">"
     ++ "a section</span></body></html>")
    sectionSyntax


sectionSyntax :: Module'
sectionSyntax = section False
    $ S.str "a section"

lookupTests = T.TestList [
    testSyntax
    ]