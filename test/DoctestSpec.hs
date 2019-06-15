{-# OPTIONS_GHC -fno-warn-missing-signatures #-}

module DoctestSpec where

import Test.DocTest
import Test.Hspec

-- $setup
-- >>> default ()
-- >>> :m +Polysemy
-- >>> :m +Polysemy.Output
-- >>> :m +Polysemy.Reader
-- >>> :m +Polysemy.State

-- |
-- >>> :{
-- foo :: Sem r ()
-- foo = put ()
-- :}
-- ...
-- ... Ambiguous use of effect 'State'
-- ...
-- ... (Member (State ()) r) ...
-- ...
ambiguousMonoState = ()

-- |
-- >>> :{
-- foo :: Sem r ()
-- foo = put 5
-- :}
-- ...
-- ... Ambiguous use of effect 'State'
-- ...
-- ... (Member (State s0) r) ...
-- ...
-- ... 's0' directly...
-- ...
ambiguousPolyState = ()

-- |
-- TODO(sandy): should this mention 'Reader i' or just 'Reader'?
--
-- >>> :{
-- interpret @Reader $ \case
--   Ask -> undefined
-- :}
-- ...
-- ... 'Reader i' is higher-order, but 'interpret' can help only
-- ... with first-order effects.
-- ...
-- ... 'interpretH' instead.
-- ...
interpretBadFirstOrder = ()

-- |
-- >>> :{
-- runFoldMapOutput
--     :: forall o m r a
--      . Monoid m
--     => (o -> m)
--     -> Sem (Output o ': r) a
--     -> Sem r (m, a)
-- runFoldMapOutput f = runState mempty . reinterpret $ \case
--   Output o -> modify (<> f o)
-- :}
-- ...
-- ... 'e10' is higher-order, but 'reinterpret' can help only
-- ... with first-order effects.
-- ...
--
-- PROBLEM: Output _is_ first order! But we're not inferring `e1 ~ Output`,
-- because the real type error breaks inference. So instead we get `e10`, which
-- we can't prove is first order, so we emit the error.
--
-- SOLUTION: Don't emit the error when `e1` is a tyvar.
firstOrderReinterpret'WRONG = ()



spec :: Spec
spec = parallel $ describe "Error messages" $ it "should pass the doctest" $ doctest
  [ "-isrc/"
  , "--fast"
  , "-XDataKinds"
  , "-XDeriveFunctor"
  , "-XFlexibleContexts"
  , "-XGADTs"
  , "-XLambdaCase"
  , "-XPolyKinds"
  , "-XRankNTypes"
  , "-XScopedTypeVariables"
  , "-XStandaloneDeriving"
  , "-XTypeApplications"
  , "-XTypeOperators"
  , "-XTypeFamilies"
  , "-XUnicodeSyntax"

  , "test/DoctestSpec.hs"

  -- Modules that are explicitly imported for this test must be listed here
  , "src/Polysemy.hs"
  , "src/Polysemy/Output.hs"
  , "src/Polysemy/Reader.hs"
  , "src/Polysemy/State.hs"
  ]

