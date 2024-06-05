module Templates.Modules.Page.Denotation where
import Templates.Modules.Page.Syntax
import Utils.Environment (FreeEnv, Env)
import Actions.Effects (MLState, ref)
import Definitions.Entity.Syntax (PName)
import Syntax (Type)
import Utils
import Templates.Effects
import Text.HTML.TagSoup (Tag(TagClose, TagOpen))

denote :: (Stream HtmlOut <: eff) =>
    Page (FreeEnv eff (Fix v))
  -> Env eff (Fix v) -> Free eff ()
-- this should do something with vars. DOesnt yet.
denote (PNavigate name vars text) env = do
    renderTag $ TagOpen "a" [("href", name)]
    renderPlainText text False
    renderTag $ TagClose "a"

    
