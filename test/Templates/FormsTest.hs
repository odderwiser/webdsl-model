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

testEq :: ()
  => String -> Out' -> Program DefSyntax (PageCall (Fix Module) Module') -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ runProgram $ foldProgram syntax

--- Tests  

testForms= testEq "test Forms" 
    (   "<html><head></head>"
     ++ "<body id=\"root\"><form accept-charset=\"UTF-8\" method=\"POST\">"
     ++ "<label>labelBool</label><input type=\"checkbox\" class=\"inputBool\">"
     ++ "<label>labelInt</label><input value=\"0\" class=\"inputInt\">"
     ++ "<label>labelStr</label><input type=\"text\" class=\"inputString\">"
     ++ "<button class=\"button\">submit</button></form></body></html>")
    formsSyntax

formsSyntax :: Program DefSyntax (PageCall (Fix Module) Module')
formsSyntax = Program 
    [ pDefEnv "root" [] [form False $ consT 
        (label (S.str "labelBool") $ input true Bool)
        $ consT (label (S.str "labelInt") $ input (int 1) Int) 
        $ consT (label (S.str "labelStr") $ input (S.str "a") S.String)  
        $ submit (int 1) (S.str "submit")]
    ]

formsTests = T.TestList 
    [ testForms
    ]