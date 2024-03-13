module Syntax where
import Bool.Syntax (LitB)
import Arith.Syntax (LitAr)
import Utils.Composition (type (<), injV, projV)
import Utils.Free (Free)

-- for now, they will be only included where they are necessary
type Address = Int

data Type  = Int | Bool 
