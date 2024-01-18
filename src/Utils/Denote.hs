module Utils.Denote where
import Utils.Free (Free)
import Utils.Composition
import Utils.Fix

type Env = [(String, Int)]

class (Functor v, Functor sym) => Denote sym eff v where
    denote :: sym (Env -> Free eff (v e)) -> (Env -> Free eff (v e))

instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where  
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (Env -> Free eff (v e)) -> Env -> Free eff (v e)
    denote a = case a of 
        (L f) -> denote f 
        (R f) -> denote f

    

foldD :: (Denote f eff v) => Fix f -> (Env -> Free eff (v e))
foldD (In f) = denote $ fmap foldD f 
