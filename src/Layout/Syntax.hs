module Layout.Syntax where

type CName = String

data Layout e f
    = Header Bool e
    | Title String
    -- | Description e  -- doesnt work anymore?
    | Section Bool e 
    | String String
    | Block Bool (Maybe CName) e
    deriving Functor

data Output e = Output e