-- {-# LANGUAGE RankNTypes #-}
module Templates.XmlTest where
import Actions.Framework
import qualified Test.HUnit as T
import Templates.Framework as Tp
import Utils
import Templates.Syntax
import Actions.Str as Str

-- --Actions
-- type Eff    = Cond + MLState Address V + End
-- type V      = Fix (LitBool + LitInt + Null + LitStr)
-- type Sym = Arith + Boolean + Expr + Eval + Str

-- --templates 
-- type Eff' = MLState Address V + RenderHtml + End
-- type Sym' = Xml

-- --running syntax
-- type Out = String
-- type Module = BiFix Sym' (Fix Sym)

-- run :: PEnv Eff Eff' V
--   -> Out
-- run e = case unwrap
--     $ handle_ renderHtmlH (PageR { title = Nothing, body = ""})
--     $ handle_ heap (makeEnv [])
--     $ e  (Env {}) (TEnv {}) handleExp
--   of
--     (_, str)    -> str


-- handleExp :: () => Free Eff V
--   -> Free Eff' V
-- handleExp e = case handle condition
--     $ e
--     of
--     exp -> bubbleDown exp

-- -- probably a beeter way to implement this??
-- bubbleDown ::
--     (eff ~> eff')
--     => Free eff v -> Free eff' v
-- bubbleDown = fold Pure (Op . cmap)

-- --- example test::

-- instance Denote Arith Eff V where
--   denote = A.denote

-- instance Denote Boolean Eff V where
--   denote = B.denote

-- instance Denote Expr Eff V where
--   denote = Ex.denote

-- instance Denote Eval Eff V where
--   denote = Ev.denote
  
-- instance Denote Str Eff V where
--   denote = Str.denote

-- instance DenoteT Xml Eff Eff' V where
--   denoteT :: Xml (FreeEnv Eff V) (PEnv Eff Eff' V) -> PEnv Eff Eff' V
--   denoteT = X.denote

testEq :: ()
  => String -> Out' -> Module' -> T.Test
testEq id res syntax =  T.TestCase $
  T.assertEqual id res $ Tp.run $ foldDT syntax

litXmlTest = testEq
    "test if simple xml works"
    ( "<html><head></head><body>"
    ++"<div id=\"header\">header()</div>"
    ++ "</body></html>")
    litXmlSyntax


litXmlSyntax :: Module'
litXmlSyntax = injBf (Xml
  "<div id=\"header\">header()</div>"
  Nothing)

recXmlTest = testEq
    "test if recursive xml works"
    ( "<html><head></head><body>"
    ++"<div id=\"pagewrapper\" />"
    ++ "</body></html>")
    recXmlSyntax


recXmlSyntax :: Module'
recXmlSyntax = injBf (Xml
  "<div id=" $
  Just (injF $ Str.Add (injS "page") (injS "wrapper"), Xml " />" Nothing))

xmlTests = T.TestList 
  [ litXmlTest
  , recXmlTest
  ]

