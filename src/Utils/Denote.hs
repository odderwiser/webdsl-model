module Utils.Denote where
import Utils.Free (Free (..), fold)
import Utils.Composition
import Utils.Fix

type Env = [(String, Int)]

class (Literal v, Functor sym) => Denote sym eff v where
    denote :: sym (Env -> Free eff (v e)) -> (Env -> Free eff (v e))

instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where  
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (Env -> Free eff (v e)) -> Env -> Free eff (v e)
    denote a = case a of 
        (L f) -> denote f 
        (R f) -> denote f

foldD :: (Denote f eff v) => Fix f -> (Env -> Free eff (v e))
foldD (In f) = denote $ fmap foldD f 

class (Functor f) => Literal f where
    coerce :: f a -> f b

instance (Literal f, Literal g) => Literal (f + g) where
  coerce :: (Literal f, Literal g) => (+) f g a -> (+) f g b
  coerce input = 
    case input of
        L f -> inj $ coerce f 
        R f -> inj $ coerce f


coerceFree :: (Literal f, Functor eff) => Free eff (f a) -> Free eff (f b)
coerceFree = fmap coerce

-- coerceFreeApplied :: (Literal f, Functor eff) 
--     => (Env -> Free eff (f a)) 
--     -> Env 
--     -> Free eff (f b)
-- coerceFreeApplied i