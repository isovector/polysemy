{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Input
  ( -- * Effect
    Input (..)

    -- * Actions
  , input

    -- * Interpretations
  , runConstInput
  , runListInput
  , runMonadicInput
  ) where

import Data.Foldable (for_)
import Data.List (uncons)
import Polysemy
import Polysemy.State

------------------------------------------------------------------------------
-- | An effect which can provide input to an application. Useful for dealing
-- with streaming input.
data Input i m a where
  Input :: Input i m i

makeSem ''Input


------------------------------------------------------------------------------
-- | Run an 'Input' effect by always giving back the same value.
runConstInput :: i -> Sem (Input i ': r) a -> Sem r a
runConstInput c = interpret $ \case
  Input -> pure c
{-# INLINE runConstInput #-}


------------------------------------------------------------------------------
-- | Run an 'Input' effect by providing a different element of a list each
-- time. Returns 'Nothing' after the list is exhausted.
runListInput
    :: [i]
    -> Sem (Input (Maybe i) ': r) a
    -> Sem r a
runListInput is = fmap snd . runState is . reinterpret
  (\case
      Input -> do
        s <- gets uncons
        for_ s $ put . snd
        pure $ fmap fst s
  )
{-# INLINE runListInput #-}


------------------------------------------------------------------------------
-- | Runs an 'Input' effect by evaluating a monadic action for each request.
runMonadicInput :: forall i r a. Sem r i -> Sem (Input i ': r) a -> Sem r a
runMonadicInput m = interpret $ \case
  Input -> m
{-# INLINE runMonadicInput #-}

