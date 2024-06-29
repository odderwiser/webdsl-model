module TemplatesTest where
import Test.HUnit 
import Templates.LayoutTest (lookupTests)
import Templates.XmlTest (xmlTests)
import Templates.TemplateTest (templateTests)
import Templates.PageTest (pageTestsIO)
import Templates.FormsTest (formsTestsIO)


templatesTests :: Test
templatesTests = TestList 
  [ lookupTests
  , xmlTests
  ]

templatesTestsIO :: IO  Test
templatesTestsIO = do
  tests <- sequence 
    [ templateTests
    , pageTestsIO
    , formsTestsIO
    ]
  return $ TestList tests