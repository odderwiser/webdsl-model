{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
module Utils.Handler where
import Utils.Free ( fold, Free(..) )
import Utils.Composition
import Control.Monad.IO.Class (MonadIO(liftIO))

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


unwrap :: Free End a -> a
unwrap (Pure x) = x
unwrap (Op f) = case f of

data Handler_ handledEff val param remEff output
  = Handler_
    { ret_  :: val -> (param -> Free remEff output)
    , hdlr_ :: handledEff (param -> Free remEff output)
      -> (param -> Free remEff output)
    }

handle_ :: (Functor handledEff, Functor remEff)
  => Handler_ handledEff val param remEff output
  -> param -> Free (handledEff + remEff) val
  -> Free remEff output

handle_ handler param value = fold
  (ret_ handler)
  (\case
     L handledEff -> hdlr_ handler handledEff
     R remainder -> \param -> Op (fmap (\apply -> apply param) remainder))
  value param

  -- defsH :: (Functor eff, Functor eff')
  -- => Handler_ (FunctionEnv eff v)
  -- a (Env eff v) eff' (a, Env eff v


data IOHandler f val g res -- can be generalised to any monad?? 
  = IOHandler
  { ioRet  :: val -> IO (Free g res)
  , ioHdlr :: f (IO (Free g res)) -> IO (Free g res)
  }

ioHandle :: ( Functor f, Functor g, Distributive g IO g)
  => IOHandler f val g res -> Free (f + g) val -> IO (Free g res)
ioHandle handler  = fold
  (ioRet handler)
  (\ x -> case x of
    L y -> ioHdlr handler y
    R y -> distr y )

class Distributive s t f where
  distr :: s (t (Free f a)) -> t (Free f a)

instance (End <: f) => Distributive End IO f where
  distr e = case e of

instance (f <: h, g <: h, Distributive f IO h,  Distributive g IO h)
  => Distributive (f + g) IO h where
  distr e = case e of
    L y -> distr y
    R y -> distr y


-- helper :: Free g (IO (Free g res)) -> IO (Free g res)
-- helper input = do
--   value <- ( do
--     input' <- input
--     _
--     )
--   return $ return _

data IOHandler_ f val param g res
  = IOHandler_
  { ioRet_  :: val -> (param -> IO (Free g res))
  , ioHdlr_ :: f (param -> IO (Free g res)) -> IO (param -> Free g res)
  }

ioHandle_ :: (Functor handledEff, Functor remEff, Distributive remEff IO remEff)
  => IOHandler_ handledEff val param remEff output
  -> param -> Free (handledEff + remEff) val
  -> IO (Free remEff output)

ioHandle_ handler param value = fold
  (ioRet_ handler)
  (\case
     L handledEff -> \param -> do
      func <- ioHdlr_ handler handledEff
      return (func param)
     R remainder -> \param -> do
        distr $ fmap (\apply -> apply param) remainder
  )
  value param