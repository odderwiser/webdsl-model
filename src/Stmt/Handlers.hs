module Stmt.Handlers where
import Utils.Denote
import Utils.Handler
import Stmt.Effects
import Stmt.Syntax
import Utils.Free
import Utils.Fix
import Utils.Composition
import Data.Maybe (mapMaybe)
import qualified Bool.Syntax as B
import Bool.Syntax (LitBool)

-- applyFilter :: (Functor eff, Functor g, [] <: a, LitBool <: a) =>
--   Handler (Apply (Free eff (Fix a)))
--   (Fix a) g (Fix a)
-- applyFilter = Handler
--   { ret = pure
--   , hdlr = \x -> case x of
--       (Apply (Where exp) (Pure k)) -> case (projF exp, projF k) of
--         (Just (exp' :: [Fix a]), Just (k' :: [Fix a])) -> Pure $ injF $ mapMaybe
--           (\(bool, val) -> case projF bool of
--             Just (B.Lit True) -> Just val 
--             _ -> Nothing ) $ zip exp' k'
--     }

filter [] vals = []
filter (True : bools) (v : vals) = v : Stmt.Handlers.filter bools vals
filter (False : bools) (v : vals) = Stmt.Handlers.filter bools vals