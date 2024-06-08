module TemplatesTest where
import Test.HUnit 
import Templates.LayoutTest (lookupTests)
import Templates.XmlTest (xmlTests)
import Templates.PageTest (pageTests)


templatesTests :: Test
templatesTests = TestList 
  [ lookupTests
  , xmlTests
  , pageTests
  ]