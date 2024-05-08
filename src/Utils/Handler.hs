module Utils.Handler where
import Utils.Free ( fold, Free(..) )
import Utils.Composition

data Handler f a f' b
  = Handler { 
    ret  :: a -> Free f' b
  , hdlr :: f (Free f' b) -> Free f' b 
  }

handle :: (Functor f, Functor f')
  => Handler f a f' b -> Free (f + f') a -> Free f' b
handle h = fold
  (ret h)
  (\ x -> case x of
    L y -> hdlr h y
    R y -> Op y)

-- permute :: (Functor f, Functor f')
--   => (f ->: f') -> Free f a -> Free f' a
-- permute f = fold Pure (Op . f)

mask :: Functor f => Free f a -> Free (f' + f) a
mask = fold Pure (Op . R)

-- hup :: f <: g => (forall f'. Functor f' => Free (f + f') a -> Free f' b)
--     -> Free g a -> Free g b
-- hup h = case forephism of
--   Forephism i -> permute (from i) . mask . h . permute (to i)

data End k
  deriving Functor

unwrap :: Free End a -> a
unwrap (Pure x) = x
unwrap (Op f) = case f of

data Handler_ handledEff val param remEff output
  = Handler_ { ret_  :: val -> (param -> Free remEff output)
             , hdlr_ :: handledEff (param -> Free remEff output) 
             -> (param -> Free remEff output) }

handle_ :: (Functor handledEff, Functor remEff)
        => Handler_ handledEff val param remEff output 
        -> Free (handledEff + remEff) val -> param -> Free remEff output

handle_ handler = fold
  (ret_ handler)
  (\case 
     L handledEff -> hdlr_ handler handledEff
     R remainder -> \param -> Op (fmap (\apply -> apply param) remainder))

flipHandle_ :: (a -> b -> c -> d) -> a -> c -> b -> d 
flipHandle_ fun a c b = fun a b c
