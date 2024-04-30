module Syntax where
import Utils.Composition (type (<), injV, projV)
import Utils.Free (Free)

-- for now, they will be only included where they are necessary
type Address = Int

data Type  = Int | Bool | List

type Null = ()
