module Templates.FormsTest where
import Templates.FrameworkIO as Tp
import qualified Test.HUnit as T
import Utils
import Definitions.Program.Syntax
import Definitions.Pages.Framework
import Definitions.Pages.Syntax
import Actions.FrameworkIO
import Templates.Syntax
import Templates.Modules.Lift.Denotation (consT, consTList)
import Syntax as S
import Actions.Values
import qualified Actions.Str as S
import Actions.Bool (true, false)
import Actions.Arith (int)
import Actions.Modules.Eval.Syntax (var)
import qualified Data.Set as Set (insert, notMember, empty, member)
import Definitions.Templates.Syntax (StatementType(Definition), TBody(Body), body)
import Definitions.Templates.Syntax (StatementType(..))
import System.Directory (removeFile)
import Definitions.Templates.Framework (EnvTy)
import qualified Templates.Modules.Layout.Syntax as L
import Templates.Modules.Phases.Syntax (Action(Action))

type Program'' = Program (Envs (PEnv (EffV V') (Eff' V') V) (EnvTy V'))  ()
  (PageCall (PEnv (EffV V') (Eff' V') V) (EnvTy V'))

testEq :: ()
  => String -> Out' -> Program DefSyntax () (PageCall Module' (Fix Module)) -> IO T.Test
testEq id res syntax = do
  let file = "./test/Templates/dbs/forms/"++id++ ".txt"
  -- removeFile file
  output <- runProgram (foldProgram syntax :: Program'') file
  return $ T.TestCase $
    T.assertEqual id res $ output

testEqId :: ()
  => String -> [HtmlOutput] -> Program DefSyntax () (PageCall Module' (Fix Module)) -> IO T.Test
testEqId id res syntax = do
  let file = "./test/Templates/dbs/forms/"++id++ ".txt"
  output <- runProgram (foldProgram syntax :: Program'') file
  let (bool, res') = compareRes res "" output Set.empty ""
  return $ T.TestCase $ T.assertEqual id (output, True) (res', bool)

--- Tests  

testForms= testEqId "test_Forms"
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

formsSyntax :: Program (Envs Module' (Fix Module)) () (PageCall Module' (Fix Module))
formsSyntax = Program
  [ pDef "root" [] $ form False $ consTList
    [ label (S.str "labelBool") $ input true Bool
    , label (S.str "labelInt") $ input (int 1) Int
    , label (S.str "labelStr") $ input (S.str "a") S.String
    , submit (action $ int 1) (S.str "submit")
    ]
  ] Nothing

formsEx :: Fix (Forms (Forms () (Fix Module)) + Module)
formsEx = injF (Form False (Label (S.str "example") ()) :: Forms (Forms () (Fix Module)) (Fix (Forms (Forms () (Fix Module)) + Module)))


formsWithVarsSyntax :: Program (Envs Module' (Fix Module)) () (PageCall Module' (Fix Module))
formsWithVarsSyntax = Program
  [ pDef "root" [] $ body ["a", "b", "c"]
    (consTList [vDeclT "a"
    , varInitT "b" true
    , varInitT "c" (S.str "a")])
    $ form False $ consTList
      [ label (S.str "labelBool") $ input (var "b") Bool
      , label (S.str "labelInt") $ input (var "a") Int
      , label (S.str "labelStr") $ input (var "c") S.String
      , submit (action $ int 1) (S.str "submit")
      ]
  ] Nothing

testDoubleLabel = testEqId "test double label" doubleLabelOutput doubleLabelSyntax

doubleLabelSyntax :: Program (Envs Module' (Fix Module)) () (PageCall Module' (Fix Module))
doubleLabelSyntax = Program
  [ pDef "root" [] $ body ["c"] (varInitT "c" (S.str "a"))
    (form False
      $ label (S.str "unused")
      $ label (S.str "used")
      $ input (var "c") S.String)
  ] Nothing

doubleLabelOutput =
  [ Plain "<html><head></head><body id=\"root\"><form id=\""
  , FormName, Plain "\" name=\""
  , FormName, Plain "\" accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Id False, Plain "\">unused</label><label for=\""
  , Id False, Plain "\">used</label><input id=\""
  , Id True ,  Plain "\" class=\"inputString\" name=\""
  , Name    , Plain "\" type=\"text\" value=\"a\"></form></body></html>" ]

noLabelSyntax :: Program (Envs Module' (Fix Module)) () (PageCall Module' (Fix Module))
noLabelSyntax = Program
  [ pDef "root" [] $ body ["c", "a"]
    (consTList [ varInitT "c" (S.str "a")
    , varInitT "a" false
    ])
    (form False $ consTList
      [ label (S.str "unused") $ label (S.str "used") $ input (var "c") S.String
      , input (var "a") Bool])
  ] Nothing

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

formsTests = 
    [ testForms
    , testFormsWithVars
    , testDoubleLabel
    , testNoLabel
    ]

formsTestsIO = do 
  tests <- sequence formsTests
  return $ T.TestList tests