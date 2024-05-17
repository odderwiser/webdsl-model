{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use lambda-case" #-}
{-# HLINT ignore "Use lambda" #-}
module Program.Handlers where
import Program.Effects
import Utils.Handler
import Utils.Environment as E
import Utils.Free
import Fun.Denotation (refDefs)
import Fun.Syntax (FDecl)
import Utils.Composition

defsHandler :: forall remEff g eff v v'. 
  (Functor eff, Functor remEff, FDecl <: g)
  => Handler (GlobalScope g eff v) v' remEff v'
defsHandler = Handler
  { ret  = pure
  , hdlr = \x -> case x of
      (Write Defs (defs :: [g (FreeEnv eff v)] ) env k)   ->
        let env' = env { E.defs = [] } in
          (case refDefs defs env' of
            (Pure env'' ) -> k env'')
  }