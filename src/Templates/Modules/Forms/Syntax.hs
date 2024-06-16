module Templates.Modules.Forms.Syntax where
import Templates.Modules.Layout.Syntax (IsAttAssigned)
import Syntax (Type)

data Param = Placeholder

data Forms e f 
    = Form IsAttAssigned f
    | Label e f -- first value evaluates to the text, the second to the field
    | Input e Type -- input with optional parameters. e is the bound writing but so far it only matters for evaluating the type of input!!
    | Submit e e -- fist thing is action, second is the string