module Col.Denotation where
import Utils.Composition
import Utils.Denote
import Utils.Free
import Col.Syntax
import Syntax
import Col.Composition
import Data.Maybe (mapMaybe)

-- elemContains e1 e2 = case projList e2 of
--       (Just (e2' :: [v])) -> return
--         $ injV
--         $ elem e1 e2'

-- -- keep this to make sure the tests pass?
-- op :: forall a v f a'. (Int < a, Bool < a, v~Recursive a, Functor f)
--   => Type -> v -> v -> Free f v
-- op ty e1 e2 = case ty of
--    Int -> elemContains e1 e2
--    Bool -> elemContains e1 e2
--    List -> case (projList e1, projList e2) of
--       (Just (e1' :: [v]) , Just (e2' :: [v])) -> return
--         $ injV
--         $ elem e1' $ mapMaybe projList e2'


-- denote :: (Eq v, Functor eff, Bool < v, Int < v)
--   => Col (FreeEnv eff (Recursive v))
--   -> FreeEnv eff (Recursive v)
-- denote (LitC v) env = do
--     v' <- mapM (\x -> x env) v
--     return $ Rec v'

-- denote (In ty a b) env = do
--     a' <- a env
--     b' <- b env
--     elemContains a' b'
