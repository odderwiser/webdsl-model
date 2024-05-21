module Layout.Syntax where
import Utils.Denote

type CName = String

data Layout f e
    = Header Bool e
    | Title String
    -- | Description e  -- doesnt work anymore?
    | Section Bool e 
    | String String
    | Block Bool (Maybe CName) e
    deriving Functor

data Output e = Output e

instance Functor' Layout where  
  gmap :: (a -> b) -> (Layout b c -> Layout b d) -> Layout a c -> Layout b d
  gmap f g (Header b e) = g (Header b e)
  gmap f g (Title s) = (Title s)
  gmap f g (Section b e) = g (Section b e)
  gmap f g (String s) = String s
  gmap f g (Block a b c) = g (Block a b c)

