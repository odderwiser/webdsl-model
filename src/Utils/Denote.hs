module Utils.Denote where
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix
import Fun.Syntax (FunName, FDecl)
import Stmt.Syntax (Filter)
import Utils.Environment (FreeEnv)

class Functor sym => Denote sym eff v where
    denote:: sym (FreeEnv eff v) -> FreeEnv eff v

instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (FreeEnv eff v) -> FreeEnv eff v
    denote a = case a of
        (L f) -> denote f
        (R f) -> denote f

class (Functor sym) => Def sym where
    foldDef :: (Def sym, Denote f eff v) => sym (Fix f) -> sym (FreeEnv eff v)

instance (Def sym1, Def sym2) => Def (sym1 + sym2) where
    foldDef a = case a of
        (L f) -> inj $ foldDef f
        (R f) -> inj $ foldDef f


foldD :: Denote f eff v => Fix f -> FreeEnv eff v
foldD (In f) = denote $ fmap foldD f

-- foldS :: Denote f eff v => Fix f -> FreeEnv eff v