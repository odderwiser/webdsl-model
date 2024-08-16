module Templates.Modules.Lift.Denotation where
import Templates.Modules.Lift.Syntax
import Actions.Syntax
import Utils as U
import Actions.Modules.Stmt.Denotation as Stmt (denoteStmt, denoteFilters) 
import Definitions.GlobalVars.Denotation (Heap)
import Actions.Bool (LitBool)
import Actions.Arith (LitInt)
import Actions.Values
import Actions.Effects
import Actions.Modules.Eval.Denotation (refEnv')

denote :: forall eff eff' v. (Functor eff, Functor eff')
  => LiftT (Stmt) (PEnv eff eff' v) (FreeEnv eff v)
  -> PEnv eff eff' v
denote (LiftT stmt@(S s1 s2)) = denoteStmt stmt

consT :: (LiftT Stmt <:: f) => BiFix f e -> BiFix f e -> BiFix f e
consT s1 s2 = injBf $ LiftT (S s1 s2)

consTList :: (LiftT Stmt <:: f) => [BiFix f e] -> BiFix f e
consTList [s1] = s1
consTList ( s1 : tail ) = injBf $ LiftT (S s1 (consTList tail))

denoteT :: forall eff eff' v v'. (Functor eff', Lift eff eff' v
  , Heap v' <: eff', Heap v' <: eff, v~ Fix v',
  LitBool <: v', LitInt <: v', Null <: v', [] <: v')
  => Loop (PEnv eff eff' v) (FreeEnv eff v) -> PEnv eff eff' v
denoteT (ForCol name col stmts filters) env = do
  col' :: v <- U.lift $ col $ actionEnv env
  col'' :: v <- U.lift $ denoteFilters name col' filters $ actionEnv env
  executeLoop (projC col'') name env stmts
  return ()

executeLoop col' name env stmts = do
  mapM_ (\id -> do
    loc <- ref id
    env' <- refEnv' name loc $ actionEnv env
    stmts env {actionEnv = env'} ) col'
