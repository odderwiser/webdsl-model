module Stmt.Effects where
import Stmt.Syntax (Filter)
import Utils.Free
import Utils.Composition

data FilterEff e k = Filter (Filter e) e k

filter f col = Op $ inj $ Filter f col $ Pure ()