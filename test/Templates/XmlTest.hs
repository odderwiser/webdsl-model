-- {-# LANGUAGE RankNTypes #-}
module Templates.XmlTest where
import Actions.Framework
import qualified Test.HUnit as T
import Templates.Framework as Tp
import Utils
import Templates.Syntax
import Actions.Str as Str

testEq :: ()
  => String -> Out' -> Module' -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ Tp.run $ (foldDT syntax :: PEnv (EffV V') (Eff' V') V)

litXmlTest = testEq
    "test if simple xml works"
    ( "<html><head></head><body>"
    ++"<div id=\"header\">header()</div>"
    ++ "</body></html>")
    litXmlSyntax


litXmlSyntax :: Module'
litXmlSyntax = xml
  "<div id=\"header\">header()</div>"

recXmlTest = testEq
    "test if recursive xml works"
    ( "<html><head></head><body>"
    ++"<div id=\"pagewrapper\" />"
    ++ "</body></html>")
    recXmlSyntax


recXmlSyntax :: Module'
recXmlSyntax = xmlRec
  "<div id=" 
  (add (Str.str "page") (Str.str "wrapper")) 
  $ xmlT " />" 

xmlTests = T.TestList 
  [ litXmlTest
  , recXmlTest
  ]

