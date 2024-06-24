module Templates.FormsTest where
import Templates.Framework as Tp
import qualified Test.HUnit as T
import Utils
import Definitions.Program.Syntax
import Definitions.Pages.Framework
import Definitions.Pages.Syntax
import Actions.Framework
import Templates.Syntax
import Templates.Modules.Lift.Denotation (consT)
import Syntax as S
import Actions.Values
import Actions.Str as S
import Actions.Bool (true, false)
import Actions.Arith (int)
import Actions.Modules.Eval.Syntax (var)
import qualified Data.Set as Set (insert, notMember, empty, member)

testEq :: ()
  => String -> Out' -> Program DefSyntax (PageCall Module' (Fix Module)) -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ runProgram $ foldProgram syntax

testEqId :: ()
  => String -> [HtmlOutput] -> Program DefSyntax (PageCall Module' (Fix Module)) -> T.Test
testEqId id res syntax =
  let
    output = runProgram $ foldProgram syntax
    (bool, res') = compareRes res "" output Set.empty ""
  in T.TestCase $ T.assertEqual id (output, True) (res', bool)

--- Tests  

testForms= testEqId "test Forms"
  (formsOutput 1)
  formsSyntax

testFormsWithVars = testEqId "test vars"
  (formsOutput 0)
  formsWithVarsSyntax

type IsEqualToLast = Bool
data HtmlOutput = Uuid String | Id IsEqualToLast
  | Name | Plain String | FormName | Button

compareRes (Plain expected : tail) lastId output idMap result =
  let (bool, res) = compareRes tail lastId (drop (length expected) output) idMap (result++expected)
  in ((expected == take (length expected) output) && bool, res)
compareRes (Uuid uuid      : tail) lastId output idMap result = compareRes tail lastId (drop 36 output) idMap (result++take 36 output)
compareRes (Id False    : tail) lastId output idMap result =
  let id = take 32 output
      (bool, res) = compareRes tail id (drop 32 output) (Set.insert id idMap) (result++ id)
  in  (Set.notMember id idMap && bool, res)
compareRes (Id True     : tail) lastId output idMap result =
  let id = take 32 output
      (bool, res) = compareRes tail "" (drop 32 output) idMap (result ++ id)
  in  (take 32 output == lastId && bool, res)
compareRes (Name     : tail) lastId output idMap result =
  let id = take 32 output
      (bool, res) = compareRes tail "" (drop 32 output) (Set.insert id idMap) (result ++ id)
  in  (Set.notMember id idMap && bool, res)
compareRes (FormName : tail) lastId output idMap result =
  let id = take 37 output
      (bool, res) = compareRes tail lastId (drop 37 output) (Set.insert (drop 5 id) idMap) (result ++ id)
  in (take 5 id == "form_" && bool, res)
compareRes (Button : tail) lastId output idMap result =
  let id = take 46 output
      (bool, res) = compareRes tail lastId (drop 46 output) idMap (result ++ id)
  in (take 12 id == "withForms_ia" && Set.member (drop 14 id) idMap && bool, res)
compareRes []                      lastId "" idMap result = (True, result)
compareRes []                      lastId oopsie idMap result = (False, result++ "oopsie: "++ oopsie)



formsOutput int =
  [ Plain "<html><head></head><body id=\"root\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Id False, Plain "\">labelBool</label><input id=\""
  , Id True , Plain  "\" class=\"inputBool\" type=\"checkbox\" name=\""
  , Name    , Plain "\" value=\"true\"><label for=\""
  , Id False, Plain "\">labelInt</label><input id=\""
  , Id True , Plain "\" class=\"inputInt\" name=\""
  , Name    , Plain $ "\" value=\""++ show int++"\"><label for=\""
  , Id False, Plain "\">labelStr</label><input id=\""
  , Id True , Plain "\" class=\"inputString\" name=\""
  , Name    , Plain $ "\" type=\"text\" value=\"a\">"
  ++ "<button class=\"button\" name=\""
  , Button  , Plain "\">submit</button></form></body></html>" ]

formsSyntax :: Program (Envs Module' (Fix Module)) (PageCall Module' (Fix Module))
formsSyntax = Program
    [ pDef "root" [] [Right $ form False $ consT
        (label (S.str "labelBool") $ input true Bool)
        $ consT (label (S.str "labelInt") $ input (int 1) Int)
        $ consT (label (S.str "labelStr") $ input (S.str "a") S.String)
        $ submit (int 1) (S.str "submit") ]
    ]

formsWithVarsSyntax :: Program (Envs Module' (Fix Module)) (PageCall Module' (Fix Module))
formsWithVarsSyntax = Program
    [ pDef "root" []
      [ Right $ form False $ consT
        (label (S.str "labelBool") $ input (var "b") Bool)
        $ consT (label (S.str "labelInt") $ input (var "a") Int)
        $ consT (label (S.str "labelStr") $ input (var "c") S.String)
        $ submit (int 1) (S.str "submit")
      , Left $ VarDeclT "a"
      , Left $ VarInit "b" true
      , Left $ VarInit "c" (S.str "a")
      ]
    ]

testDoubleLabel = testEqId "test double label" doubleLabelOutput doubleLabelSyntax

doubleLabelSyntax :: Program (Envs Module' (Fix Module)) (PageCall Module' (Fix Module))
doubleLabelSyntax = Program
    [ pDef "root" []
      [ Right $ form False $ label (S.str "unused") $ label (S.str "used") $ input (var "c") S.String
      , Left $ VarInit "c" (S.str "a")
      ]
    ]

doubleLabelOutput =
  [ Plain "<html><head></head><body id=\"root\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Id False, Plain "\">unused</label><label for=\""
  , Id False, Plain "\">used</label><input id=\""
  , Id True ,  Plain "\" class=\"inputString\" name=\""
  , Name    , Plain "\" type=\"text\" value=\"a\"></form></body></html>" ]

noLabelSyntax :: Program (Envs Module' (Fix Module)) (PageCall Module' (Fix Module))
noLabelSyntax = Program
  [ pDef "root" []
    [ Right $ form False $ consT (label (S.str "unused") $ label (S.str "used") $ input (var "c") S.String)
      (input (var "a") Bool)
    , Left $ VarInit "c" (S.str "a")
    , Left $ VarInit "a" false
    ]
  ]

testNoLabel = testEqId "test no label" noLabelOutput noLabelSyntax

noLabelOutput =
  [ Plain     "<html><head></head><body id=\"root\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Id False, Plain "\">unused</label><label for=\""
  , Id False, Plain "\">used</label><input id=\""
  , Id True,  Plain "\" class=\"inputString\" name=\""
  , Name    , Plain "\" type=\"text\" value=\"a\">"
  , Plain   "<input class=\"inputBool\" type=\"checkbox\" name=\""
  , Name    , Plain "\" value=\"false\">"
  , Plain   "</form></body></html>" ]

formsTests = T.TestList
    [ testForms
    , testFormsWithVars
    , testDoubleLabel
    , testNoLabel
    ]