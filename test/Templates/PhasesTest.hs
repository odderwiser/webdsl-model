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
import Actions.Syntax
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
    return $ T.TestList [
       test1
      ,test2
      , test3
      ]
