-- {-# LANGUAGE QuantifiedConstraints #-}
-- module Eval.Effects where
-- import Utils.Composition
-- import Utils.Free (Free(..))

-- data MLState m v k
--   = Ref v (m -> k)
--   | Deref m (v -> k)
--   | Assign (m, v) k
--   deriving Functor

-- deref :: MLState m v <: f => m -> Free f v
-- deref key = Op $ inj $ Deref key Pure

-- assign :: MLState m v <: f => (m, v) -> Free f ()
-- assign pair = Op $ inj $ Assign pair $ Pure ()

-- ref :: MLState m v <: f => v -> Free f m
-- ref val = Op $ inj $ Ref val Pure

-- -- class Cmap s where
-- --   cmap :: forall f a. (s <: f) => Free s a -> Free f a

-- -- -- this is enough
-- -- instance Cmap (MLState m v) where
-- --   cmap :: (MLState m v ~ s, s <: f) => Free s a -> Free f a
-- --   cmap (Pure s) = Pure s
-- --   cmap (Op (Ref v f)) = Op $ inj $ Ref v $ cmap . f
-- --   cmap (Op (Deref k f)) = Op $ inj $ Deref k $ cmap . f
-- --   cmap (Op (Assign r k)) = Op $ inj $ Assign r $ cmap k

-- -- class Cmap' s where
-- -- -- natural transformation??
-- --   cmap' :: forall f a. (s <: f)
-- --     => s (Free s a) -> Free f a

-- -- --these classes probably wont work anyway
-- -- instance Cmap' (MLState m v) where
-- --   cmap' (Ref v f) = Op $ inj $ Ref v $ cmap . f
-- --   cmap' (Deref k f) = Op $ inj $ Deref k $ cmap . f
-- --   cmap' (Assign t k) = Op $ inj $ Assign t $ cmap k

-- -- instance (Cmap s, Cmap g, Cmap' (s + g)) => Cmap (s + g) where
-- --   cmap :: (Cmap s, Cmap g) => Free (s + g) a -> Free f a
-- --   cmap (Pure s) = (Pure s)
-- --   cmap (Op a) = case a of
-- --     (L f) -> Op $ inj $ cmap' f

-- -- instance (Cmap' s, Cmap' g) => Cmap' (s + g) where
-- --   cmap' x = case x of
-- --     (L f) -> cmap' f
