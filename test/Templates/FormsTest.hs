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

--- Tests  

testForms= testEq "test Forms" 
  (formsOutput 1)
  formsSyntax

testFormsWithVars = testEq "test vars"
  (formsOutput 0)
  formsWithVarsSyntax

formsOutput int = "<html><head></head>"
  ++ "<body id=\"root\"><form accept-charset=\"UTF-8\" method=\"POST\">"
  ++ "<label>labelBool</label><input type=\"checkbox\" class=\"inputBool\" value=\"true\">"
  ++ "<label>labelInt</label><input value=\""++ show int++"\" class=\"inputInt\">"
  ++ "<label>labelStr</label><input type=\"text\" value=\"a\" class=\"inputString\">"
  ++ "<button class=\"button\">submit</button></form></body></html>"

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