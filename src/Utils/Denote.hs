module Utils.Denote where
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix
import Eval.Syntax
import Syntax
import Fun.Syntax (FunName, FDecl)

type Function eff v = FDecl (Env eff v -> Free eff v)
type FreeEnv eff v = Env eff v -> Free eff v

data Env eff v 
    = Env [(VName, Address)] 
    | Defs [(FunName, Function eff v)]
    | EList [Env eff v]

class Functor sym => Denote sym eff v where
    denote :: sym (FreeEnv eff v) -> FreeEnv eff v

instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (FreeEnv eff v) -> FreeEnv eff v
    denote a = case a of
        (L f) -> denote f
        (R f) -> denote f

foldD :: Denote f eff v => Fix f -> FreeEnv eff v
foldD (In f) = denote $ fmap foldD f
