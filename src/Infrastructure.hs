class Denote sym eff v where
    denote :: sym (env -> Free eff v) -> (env -> Free eff v)

instance (Denote sym1 eff v, Denote sym2 eff v) => Denote (sym1 + sym2) eff v