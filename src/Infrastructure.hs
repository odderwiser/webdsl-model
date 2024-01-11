module Infrastructure where

import Free

class Functor sym => Denote sym eff v where
    denote :: sym (env -> Free eff v) -> (env -> Free eff v)

-- instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where
--   denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
--     (+) sym1 sym2 (Free eff v) -> Free eff v
--   denote a = case a of 
--         (L f) -> denote f 
--         (R f) -> denote f

instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where  
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (env -> Free eff v) -> env -> Free eff v
    denote a = case a of 
        (L f) -> denote f 
        (R f) -> denote f