module TemplatesTest where
import Test.HUnit 
import Templates.LayoutTest (lookupTests)
import Templates.XmlTest (xmlTests)
import Templates.TemplateTest (templateTests)
import Templates.PageTest (pageTests)
import Templates.FormsTest (testForms)


templatesTests :: Test
templatesTests = TestList 
  [ lookupTests
  , xmlTests
  , templateTests
  , pageTests
  , testForms
  ]