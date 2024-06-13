module Utils.Denote where
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix
import Utils.Environment (FreeEnv, PEnv, Env)
import Data.Bifunctor (Bifunctor (bimap))


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


class (Functor eff', Functor sym) => DenoteDef sym e eff' where
    denoteDef :: sym e -> Free eff' ()
    denoteDefList ::  [sym e] -> Free eff' [()]
    denoteDefList = mapM denoteDef


instance  (DenoteDef sym1 e eff', DenoteDef sym2 e eff')
    => DenoteDef (sym1 + sym2) e eff' where
    denoteDef :: (DenoteDef sym1 e eff', DenoteDef sym2 e eff') =>
        (+) sym1 sym2 e -> Free eff' ()
    denoteDef a = case a of
        (L f) -> denoteDef f
        (R f) -> denoteDef f

foldDef (In f) = denoteDef f



class DenoteT sym eff eff' v where
    denoteT :: sym (FreeEnv eff v) (PEnv eff eff' v) -> PEnv eff eff' v 

instance  (DenoteT sym1 eff eff' v,
    DenoteT sym2 eff eff' v)
    => DenoteT (sym1 +: sym2) eff eff' v where
    denoteT a = case a of
        (L' f) -> denoteT f
        (R' f) -> denoteT f

-- data (Functor f) => Fix' e f = In' (e (Fix f) (Fix' e f))

foldDT :: (Denote f eff v, DenoteT g eff eff' v, Bifunctor g)
    => BiFix g (Fix f) -> PEnv eff  eff' v 
foldDT (BIn elem) = denoteT $ bimap foldD foldDT elem



-- foldS :: Denote f eff v => Fix f -> FreeEnv eff v

class (Functor eff, Functor eff') => Lift eff eff' v where
    lift :: Free eff v -> Free eff' v