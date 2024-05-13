module Stmt.Effects where
import Stmt.Syntax (Filter)
import Utils.Free
import Utils.Composition

data FilterEff e k = Filter (Filter e) e k

data Apply e k = Apply (Filter e) k

apply f list = Op $ inj $ Apply f list

filter f col = Op $ inj $ Filter f col $ Pure ()