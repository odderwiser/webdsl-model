{-# LANGUAGE EmptyCase #-}
module LayoutTest where
import Utils.Handler 
import Utils.Fix
import Syntax
import Layout.Effects
import Attributes.Syntax
import Utils.Composition
import Layout.Syntax
import Utils.Environment
import Layout.Handlers
import qualified Test.HUnit as T
import Utils.Denote (foldDT)
import Utils.Denote
import qualified Layout.Denotation as L

--Actions
type Eff = End
type V =  (Fix End)

--templates 
type Eff' = Attribute + RenderHtml + State (AttList String) + End

--running syntax
type Module = Fix' Layout V   
type Out = String

run :: PEnv Eff Eff' (V)
  -> Out
run e = case unwrap
    $ handle_ stateH []
    $ handle_ renderHtmlH (PageR { title = Nothing, body = ""})
    $ handle_ attributeH ("section", 1)
    $ e  (Env {}) (TEnv {})
  of
    (_, str)    -> str

--- example test::

instance Denote End Eff V where
  denote :: End (FreeEnv  Eff V)
    -> FreeEnv Eff V
  denote = \x -> case x of

instance DenoteT Layout Eff Eff' V where
  denoteT :: Layout (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
  denoteT = L.denote

testEq :: () 
  => String -> Out -> Module -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ run $ foldDT syntax

testSyntax = testEq
    "test if templates work"
    (   "<html><head></head><body>"
     ++ "<span class=\"section section1\">"
     ++ "a section</span></body></html>")
    sectionSyntax


sectionSyntax :: Module
sectionSyntax = injF'Fst (Section False 
    $ injF'Fst (String "a section"))

lookupTests = T.TestList [
    testSyntax
    ]