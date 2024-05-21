module Utils.Denote where
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix
import Fun.Syntax (FunName, FDecl)
import Stmt.Syntax (Filter)
import Utils.Environment (FreeEnv, PEnv)

class Functor sym => Denote sym eff v where
    denote:: sym (FreeEnv eff v) -> FreeEnv eff v

instance  (Denote sym1 eff v, Denote sym2 eff v)
    => Denote (sym1 + sym2) eff v where
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (FreeEnv eff v) -> FreeEnv eff v
    denote a = case a of
        (L f) -> denote f
        (R f) -> denote f


foldD :: Denote f eff v => Fix f -> FreeEnv eff v
foldD (In f) = denote $ fmap foldD f


class (Functor eff, Functor eff', Functor sym) => DenoteDef sym eff eff' v where
    denoteDef :: sym (FreeEnv eff v) -> Free eff' ()
    denoteDefList ::  [sym (FreeEnv eff v)] -> Free eff' [()]
    denoteDefList = mapM denoteDef


instance  (DenoteDef sym1 eff eff' v, DenoteDef sym2 eff eff' v)
    => DenoteDef (sym1 + sym2) eff eff' v where
    denoteDef a = case a of
        (L f) -> denoteDef f
        (R f) -> denoteDef f

class DenoteT sym eff eff' v where
    denoteT :: sym (FreeEnv eff v) (PEnv eff eff' v) -> PEnv eff eff' v

instance  (DenoteT sym1 eff eff' v,
    DenoteT sym2 eff eff' v)
    => DenoteT (sym1 +: sym2) eff eff' v where
    denoteT a = case a of
        (L' f) -> denoteT f
        (R' f) -> denoteT f

-- data (Functor f) => Fix' e f = In' (e (Fix f) (Fix' e f))

foldDT :: (Denote f eff v, DenoteT g eff eff' v, Functor' g, Functor (g (FreeEnv eff v)))
    => Fix' g (Fix f) -> PEnv eff  eff' v
foldDT (In' elem) = denoteT $ gmap (foldD) (fmap foldDT) elem

class Functor' f  where
    gmap :: (a -> b) -> (f b c -> f b d) -> f a c -> f b d
    -- (<$) :: a -> f b -> f a



-- foldS :: Denote f eff v => Fix f -> FreeEnv eff v