module Syntax where
import Utils.Composition (type (<:))
import Utils.Free (Free)
import Utils.Fix
import GHC.Generics (Generic)
import Data.Aeson (ToArgs, ToJSON)
-- for now, they will be only included where they are necessary
type Address = Int

data Type  = Int | Bool | List | NullType | Entity String | String
    deriving (Eq, Show)
