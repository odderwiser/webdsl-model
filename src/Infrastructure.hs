module Infrastructure where

import Free

type Env = [(String, Int)]

-- denote   

class Functor sym => Denote sym eff v where
    denote :: sym (Env -> Free eff v) -> (Env -> Free eff v)

-- instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where
--   denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
--     (+) sym1 sym2 (Free eff v) -> Free eff v
--   denote a = case a of 
--         (L f) -> denote f 
--         (R f) -> denote f

instance  (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v where  
    denote :: (Denote sym1 eff v, Denote sym2 eff v) =>
        (+) sym1 sym2 (Env -> Free eff v) -> Env -> Free eff v
    denote a = case a of 
        (L f) -> denote f 
        (R f) -> denote f

-- fix point

data Fix f = In (f (Fix f))


class (f <: g) => BinaryInject f g op where
    bin ::  op -> f (Fix g) -> f (Fix g) -> Fix g