module TemplatesTest where
import Test.HUnit 
import Templates.LayoutTest (lookupTests)
import Templates.XmlTest (xmlTests)


templatesTests :: Test
templatesTests = TestList 
  [ lookupTests
  , xmlTests
  ]