module Templates.Modules.Lift.Denotation where
import Templates.Modules.Lift.Syntax
import Actions.Syntax
import Utils
import Actions.Modules.Stmt.Denotation (denoteStmt)

denote :: forall eff eff' v. (Functor eff, Functor eff')
  => LiftT (Stmt) (FreeEnv eff v) (PEnv eff eff' v)
  -> PEnv eff eff' v
denote (LiftT stmt@(S s1 s2)) = denoteStmt stmt
