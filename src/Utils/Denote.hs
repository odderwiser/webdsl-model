module Utils.Denote where
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix
import Utils.Environment (FreeEnv, PEnv, Env, TEnv (actionEnv))
import Data.Bifunctor (Bifunctor (bimap))
import Templates.Modules.Lift.Syntax (LiftT (..), LiftE (LiftE))


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

denoteDefList :: (DenoteDef sym e eff') => [sym e] -> Free eff' ()
denoteDefList = mapM_ denoteDef

class (Functor eff', Bifunctor sym) => DenoteDef' sym f e eff' where
    denoteDef' :: sym f e  -> Free eff' ()

denoteDefList' :: (DenoteDef' sym f e eff') =>  [sym f e] -> Free eff' ()
denoteDefList' = mapM_ denoteDef'

instance DenoteDef sym e eff' => DenoteDef' (LiftE sym) f e eff' where
  denoteDef' (LiftE x)  = denoteDef x

instance  (DenoteDef sym1 e eff', DenoteDef sym2 e eff')
    => DenoteDef (sym1 + sym2) e eff' where
    denoteDef :: (DenoteDef sym1 e eff', DenoteDef sym2 e eff') =>
        (+) sym1 sym2 e -> Free eff' ()
    denoteDef a = case a of
        (L f) -> denoteDef f
        (R f) -> denoteDef f

instance  (DenoteDef' sym1 e f eff', DenoteDef' sym2 e f eff')
    => DenoteDef' (sym1 +: sym2) e f eff' where
    denoteDef' a = case a of
        (L' f) -> denoteDef' f
        (R' f) -> denoteDef' f

foldDef (In f) = denoteDef f



class DenoteT sym eff eff' v where
    denoteT :: sym (PEnv eff eff' v) (FreeEnv eff v) -> PEnv eff eff' v

instance  (DenoteT sym1 eff eff' v,
    DenoteT sym2 eff eff' v)
    => DenoteT (sym1 +: sym2) eff eff' v where
    denoteT a = case a of
        (L' f) -> denoteT f
        (R' f) -> denoteT f

instance (Denote sym eff v, Functor eff', Lift eff eff' v) => DenoteT (LiftE sym) eff eff' v where
  denoteT (LiftE x) env = do
    v <- lift $ denote x (actionEnv env)
    return ()

-- data (Functor f) => Fix' e f = In' (e (Fix f) (Fix' e f))

foldDT :: (Denote f eff v, DenoteT g eff eff' v, Bifunctor g)
    => BiFix g f -> PEnv eff  eff' v
foldDT (BIn elem) = denoteT $ bimap foldDT foldD elem



-- foldS :: Denote f eff v => Fix f -> FreeEnv eff v

class (Functor eff, Functor eff') => Lift eff eff' v where
    lift :: Free eff v -> Free eff' v