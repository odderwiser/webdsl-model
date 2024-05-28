module Actions.Modules.Col.Syntax where
import Syntax (Type)
import Utils.Composition
import Utils.Fix
import Eval.Syntax (VName)
import Stmt.Syntax
import Data.Maybe (fromJust)
import Actions.Modules.Bool.Syntax (OpB)

data Col e 
  = LitC [e]                   -- List literal : []
  | OpIn e e                   -- Contains: e1 in e2
  | LComp e VName e [Filter e] -- List Comprehension: [ e1 | VName : type in e2 filter ]. Syntactic sugar for ForLoop?
  | UnOp OpB e                 -- List Comp operation: And [ e1 | VName : type in e2 filter ]
    deriving Functor

injC :: (Col <: f) => [Fix f] -> Fix f
injC =  injF . LitC

projC :: ([] <: g) => Fix g -> [Fix g]
projC col = fromJust (projF col)