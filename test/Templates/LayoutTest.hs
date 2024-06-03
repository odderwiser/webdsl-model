{-# LANGUAGE EmptyCase #-}
module Templates.LayoutTest where
import qualified Test.HUnit as T
import Utils
import Actions.Framework   as Ac 
import Templates.Framework   as Tp 
import Templates.Syntax
import Actions.Arith (Arith)
import Actions.Bool (Boolean)
import Actions.Syntax
import Actions.Str
import Templates.Effects
import Templates.Handlers.Render
import Templates.Handlers.Layout
import Templates.Modules.Layout.Denotation as L
import Actions.Effects
import Syntax (Address)
import Actions.Handlers.Heap (heap, makeEnv)

import Actions.Modules.Arith.Syntax as A
import Actions.Modules.Bool.Syntax as B
import Actions.Modules.Col.Syntax as C
import Actions.Modules.Str.Syntax as Str
import Actions.Syntax
import Actions.Handlers.Return
import Actions.Handlers.Cond (condition)
import Actions.Handlers.Heap

--- example test::

-- instance Denote End Eff V where
--   denote :: End (FreeEnv  Eff V)
--     -> FreeEnv Eff V
--   denote = \x -> case x of


testEq :: () 
  => String -> Out' -> Module' -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ Tp.run $ foldDT syntax

foldSemantics :: () 
  => Module' -> PEnv Eff Eff' V
foldSemantics syntax = foldDT syntax

testSyntax = testEq
    "test if templates work"
    (   "<html><head></head><body>"
     ++ "<span class=\"section section1\">"
     ++ "a section</span></body></html>")
    sectionSyntax


sectionSyntax :: Module'
sectionSyntax = injBf (Section False 
    $ injBf (String "a section"))

lookupTests = T.TestList [
    testSyntax
    ]