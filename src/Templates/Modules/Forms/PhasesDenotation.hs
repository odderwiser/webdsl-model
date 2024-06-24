module Templates.Modules.Forms.PhasesDenotation where
import Templates.Effects as E
import Utils
import Actions.Effects
import Actions.Bool (LitBool)
import Actions.Arith (LitInt)
import Actions.Str (LitStr)
import Templates.Modules.Forms.Syntax


denoteDb :: forall eff eff' v v'.
  ( E.Attribute <: eff', Stream HtmlOut <: eff'
  , State (Maybe LabelId) <: eff', Random Label LabelId <: eff'
  , State Seed <: eff'
  , LitStr <: v', LitBool <: v', LitInt <: v' , v ~ Fix v'
  , Lift eff eff' v)
  => Forms (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denoteDb _ env = return ()