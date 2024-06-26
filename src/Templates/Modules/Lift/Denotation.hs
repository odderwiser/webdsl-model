module Templates.Modules.Lift.Denotation where
import Templates.Modules.Lift.Syntax
import Actions.Syntax
import Utils
import Actions.Modules.Stmt.Denotation (denoteStmt)

denote :: forall eff eff' v. (Functor eff, Functor eff')
  => LiftT (Stmt) (PEnv eff eff' v) (FreeEnv eff v) 
  -> PEnv eff eff' v
denote (LiftT stmt@(S s1 s2)) = denoteStmt stmt

consT :: (LiftT Stmt <:: f) => BiFix f e -> BiFix f e -> BiFix f e
consT s1 s2 = injBf $ LiftT (S s1 s2)

consTList :: (LiftT Stmt <:: f) => [BiFix f e] -> BiFix f e
consTList [s1] = s1
consTList ( s1 : tail ) = injBf $ LiftT (S s1 (consTList tail))