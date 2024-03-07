module Expr where
import Effects (Cond)
import Utils.Composition (type (+), type (\/), End)
import Utils.Denote (Env)
import Utils.Free (Free)
import Arith.Syntax as A ( LitAr(..), Arith )
import Arith.Interface as A ( denote )
import Bool.Syntax as B ( LitB(..), Boolean )
import Bool.Interface as B
import Expr.Interface as E
import Expr.Syntax
import Utils.Handler (unwrap, handle)
import Bool.Handlers (condition)
import Utils.Denote (Denote(denote))

type Eff = Cond + End
type V =  LitB \/ LitAr

run :: (Env -> Free Eff V)
  -> Either Bool Int
run e = case unwrap
    $ handle condition
    $ e []
  of 
    (Left (B.Lit val))  -> Left val
    (Right (A.Lit val)) -> Right val

instance Denote Arith Eff V where
  denote = A.denote

instance Denote Boolean Eff V where
  denote = B.denote

instance Denote Expr Eff V where
  denote = E.denote