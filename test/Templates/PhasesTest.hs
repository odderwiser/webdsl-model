module Templates.PhasesTest where
import Templates.FrameworkIO
import Definitions.Program.Syntax
import Definitions.Pages.Syntax
import Utils
import Definitions.Pages.Framework (DefSyntax)
import Actions.FrameworkIO
import qualified Test.HUnit as T
import Templates.FormsTest as Forms (HtmlOutput (..), compareRes)
import qualified Data.Set as Set
import PhasesFramework.Framework as F
import Definitions.GlobalVars.ActionsFramework
import Definitions.Syntax as D
import Templates.Syntax
import qualified Actions.Str as S
import Actions.Syntax as S
import Syntax as S
import Actions.Arith
import System.Directory (doesFileExist, removeFile)
import Control.Monad
import Templates.Modules.Lift.Denotation (consTList)
import qualified Templates.Modules.Layout.Syntax as L
import Templates.Effects (redirect)
import Actions.Modules.Phases.Syntax (redirectS)

testEq :: ()
  => String -> Out' -> Program DefSyntax (Fix Sym) (PageCall Module' (Fix Module)) -> IO T.Test
testEq id res syntax = do
  let file = "./test/Templates/dbs/phases/"++id++ ".txt"
  -- removeFile file
  fileExists <- doesFileExist file
  when fileExists $ removeFile file
  output <- F.runProgram syntax file
  print output
  return $ T.TestCase $
    T.assertEqual id res $ output

testEqId :: ()
  => String -> [HtmlOutput] -> Program DefSyntax (Fix Sym) (PageCall Module' (Fix Module)) -> IO T.Test
testEqId id res syntax = do
  let file = "./test/Templates/dbs/phases/"++id++ ".txt"
  fileExists <- doesFileExist file
  when fileExists $ removeFile file
  output <- F.runProgram syntax file
  print output
  let (bool, res') = compareRes res "" output Set.empty ""
  return $ T.TestCase $ T.assertEqual id (output, True) (res', bool)

---- 

testProperty = testEqId "property" testPropertyOutput testPropertySyntax

testPropertySyntax :: Program DefSyntax (Fix Sym) (PageCall Module' (Fix Module))
testPropertySyntax = Request
  [ pDef "root" []
    (form False
      $ label (S.str "someLabel")
      $ input (pAccess (var "left") "a") S.Int)
    , eDef' "obj" [("a", Int)] [] []
  ] (vList [VDef "left" (EDecl "obj" [("a", int 1)])]) (PCall "root" []
    , [ ("form_92d1d547132b3a579f28b944be2c0ff9", "1")
      , ("withForms_ia0_c7d3fe12397d3a6ea3b9f7a6db2d6190", "10")
      ])

testButton = testEqId "button" testButtonOutput testPropertyButtonSyntax

testPropertyButtonSyntax :: Program DefSyntax (Fix Sym) (PageCall Module' (Fix Module))
testPropertyButtonSyntax = Request
  [ pDef "root" []
    (form False $ consTList [
      label (S.str "someLabel")
      $ input (pAccess (var "left") "a") S.Int,
      submit (action $ save (var "left")) (S.str "save")
      ])
    , eDef' "obj" [("a", Int)] [] []
  ] (vList [VDef "left" (EDecl "obj" [("a", int 1)])]) (PCall "root" []
    , [ ("form_92d1d547132b3a579f28b944be2c0ff9", "1")
      , ("f6780915ddea3af5817361282fc33576", "10")
      , ("withForms_ia0_92d1d547132b3a579f28b944be2c0ff9", "save")
      ])


testRedirect = testEq "redirect" testRedirectOutput testRedirectSyntax

testRedirectSyntax :: Program DefSyntax (Fix Sym) (PageCall Module' (Fix Module))
testRedirectSyntax = Request
  [ pDef "root" []
    (form False $ consTList [
      label (S.str "someLabel")
      $ input (pAccess (var "left") "a") S.Int,
      submit (action $ redirectS "goal") (S.str "save")
      ])
    , pDef "goal" [] $ section False $ L.str "success!"
    , eDef' "obj" [("a", Int)] [] []
  ] (vList [VDef "left" (EDecl "obj" [("a", int 1)])]) (PCall "root" []
    , [ ("form_92d1d547132b3a579f28b944be2c0ff9", "1")
      , ("f6780915ddea3af5817361282fc33576", "10")
      , ("withForms_ia0_92d1d547132b3a579f28b944be2c0ff9", "save")
      ])

testRedirectOutput = "<html><head></head><body id=\"goal\">"
     ++ "<span class=\"section section1\">"
     ++ "success!</span></body></html>"


systemTestSyntax :: Program DefSyntax (Fix Sym) (PageCall Module' (Fix Module))
systemTestSyntax = Request
  [ eDef' "Project" [("name", S.String), ("apps", S.List (Entity "Project"))] [] 
    [S.Validate (gt (S.length (pVar "name")) (int 0)) "Name must be longer than 0" ["name"]],
    pDef "editProject" [("p", Entity "Project")] $ form False $ consTList 
      [ output $ pAccess (var "p") "name"
      , forAllT "app" (pAccess (var "p") "apps") 
        $ input (pAccess (var "app") "name") S.String
      , submit (action $ forAll "app" 
        (pAccess (var "p") "apps") (save (var "app")) [] ) 
        (S.str "save")
    ] ]
  (vList [VDef "webdsl" $ EDecl "Project" [("name",S.str "WebDSL")
      , ("apps", list [var ("yg"), var("wl"), var "cf", var "re"])],
      VDef "yg" $ EDecl "Project" [("name", S.str "yellowgrass")],
      VDef "wl" $ EDecl "Project" [("name", S.str "weblab")],
      VDef "cf" $ EDecl "Project" [("name", S.str "codefinder")],
      VDef "re" $ EDecl "Project" [("name", S.str "researchr")]  ])
  (PCall "editProject" [(var "webdsl", Entity "Project")], [
     ("form_92d1d547132b3a579f28b944be2c0ff9", "1")
     , ("dd853912a9de30debe8e666fda7b2252", "Yellowgrass")
     , ("796c994b9bda387a86eca718bcefa3b2", "Weblab")
     , ("810751c35afe38679b6905198b41c878", "Codefinder")
     , ("f82e5e76bd0a363ba77b5d6fecfcb922", "Researchr")
     , ("withForms_ia0_92d1d547132b3a579f28b944be2c0ff9", "save")
    ])

systemTestOutput = [ Plain "<html><head></head><body id=\"editProject\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\">WebDSL<input class=\"inputString\" name=\""
  , Name    , Plain $ "\" type=\"text\" value=\"Yellowgrass\"><input class=\"inputString\" name=\""
  , Name    , Plain $ "\" type=\"text\" value=\"Weblab\"><input class=\"inputString\" name=\""
  , Name    , Plain $ "\" type=\"text\" value=\"Codefinder\"><input class=\"inputString\" name=\""
  , Name    , Plain $ "\" type=\"text\" value=\"Researchr\"><button class=\"button\" name=\""
  , Button  , Plain "\">save</button></form></body></html>" ]

systemTest = testEqId "systemTest" systemTestOutput systemTestSyntax

testPropertyOutput =
  [ Plain "<html><head></head><body id=\"root\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Forms.Id False, Plain "\">someLabel</label><input id=\""
  , Forms.Id True , Plain "\" class=\"inputInt\" name=\""
  , Name    , Plain $ "\" value=\"1\"></form></body></html>" ]

testButtonOutput =
  [ Plain "<html><head></head><body id=\"root\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Forms.Id False, Plain "\">someLabel</label><input id=\""
  , Forms.Id True , Plain "\" class=\"inputInt\" name=\""
  , Name    , Plain "\" value=\"10\"><button class=\"button\" name=\""
  , Button  , Plain "\">save</button></form></body></html>" ]


phasesTests = do
    test1 <- testProperty
    test2 <- testButton
    test3 <- testRedirect
    test4 <- systemTest
    return $ T.TestList [
       test1
      ,test2
      , test3
      , test4
      ]
