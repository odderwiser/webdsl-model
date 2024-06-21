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
import Actions.Bool (true)
import Actions.Arith (int)
import Actions.Modules.Eval.Syntax (var)

testEq :: ()
  => String -> Out' -> Program DefSyntax (PageCall Module' (Fix Module)) -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ runProgram $ foldProgram syntax

testEqId :: ()
  => String -> [HtmlOutput] -> Program DefSyntax (PageCall Module' (Fix Module)) -> T.Test
testEqId id res syntax = 
  let 
    output = runProgram $ foldProgram syntax
    (bool, res') = compareRes res "" output ""
  in T.TestCase $ T.assertEqual id (output, True) (res', bool)

--- Tests  

testForms= testEqId "test Forms"
  (formsOutput 1)
  formsSyntax

testFormsWithVars = testEqId "test vars"
  (formsOutput 0)
  formsWithVarsSyntax

data HtmlOutput = Uuid String | Id Bool | Plain String

-- compareRes :: [HtmlOutput] -> String -> Bool
compareRes :: [HtmlOutput] -> [Char] -> [Char] -> [Char] -> (Bool, String)
compareRes (Plain expected : tail) lastId output result = 
  let (bool, res) = compareRes tail lastId (drop (length expected) output) (result++expected)
  in ((expected == take (length expected) output) && bool, res)
compareRes (Uuid uuid      : tail) lastId output result = compareRes tail lastId (drop 36 output) (result++take 36 output)
compareRes (Id False    : tail) lastId output result = compareRes tail (take 32 output) (drop 32 output) (result++ take 32 output)
compareRes (Id True     : tail) lastId output result = 
  let (bool, res) = compareRes tail "" (drop 32 output) (result ++ take 32 output)
  in  (take 32 output == lastId && bool, res)
compareRes []                      lastId "" result = (True, result)
compareRes []                      lastId oopsie result = (False, result++ "oopsie: "++ oopsie) 



formsOutput int = 
  [ Plain "<html><head></head><body id=\"root\"><form accept-charset=\"UTF-8\" method=\"POST\"><label for=\""
  , Id False, Plain "\">labelBool</label><input id=\""
  , Id True,  Plain  "\" type=\"checkbox\" class=\"inputBool\" value=\"true\"><label for=\""
  , Id False, Plain "\">labelInt</label><input id=\""
  , Id True,  Plain $ "\" value=\""++ show int++"\" class=\"inputInt\"><label for=\""
  , Id False, Plain "\">labelStr</label><input id=\""
  , Id True,  Plain $ "\" type=\"text\" value=\"a\" class=\"inputString\">"
  ++ "<button class=\"button\">submit</button></form></body></html>" ]

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

formsTests = T.TestList
    [ testForms
    , testFormsWithVars
    ]