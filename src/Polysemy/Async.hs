{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Async
  ( -- * Effect
    Async (..)

    -- * Actions
  , async
  , await

    -- * Interpretations
  , asyncToIO
  , lowerAsync
  ) where

import qualified Control.Concurrent.Async as A
import           Polysemy



------------------------------------------------------------------------------
-- | An effect for spawning asynchronous computations.
--
-- The 'Maybe' returned by 'async' is due to the fact that we can't be sure an
-- 'Polysemy.Error.Error' effect didn't fail locally.
--
-- @since 0.5.0.0
data Async m a where
  Async :: m a -> Async m (A.Async (Maybe a))
  Await :: A.Async a -> Async m a

makeSem ''Async

------------------------------------------------------------------------------
-- | A more flexible --- though less performant ---  version of 'lowerAsync'.
--
-- This function is capable of running 'Async' effects anywhere within an
-- effect stack, without relying on an explicit function to lower it into 'IO'.
-- Notably, this means that 'Polysemy.State.State' effects will be consistent
-- in the presence of 'Async'.
--
-- @since 0.5.0.0
asyncToIO
    :: LastMember (Embed IO) r
    => Sem (Async ': r) a
    -> Sem r a
asyncToIO m = withLowerToIO $ \lower _ -> lower $
  interpretH
    ( \case
        Async a -> do
          ma  <- runT a
          ins <- getInspectorT
          fa  <- embed $ A.async $ lower $ asyncToIO ma
          pureT $ fmap (inspect ins) fa

        Await a -> pureT =<< embed (A.wait a)
    )  m
{-# INLINE asyncToIO #-}


------------------------------------------------------------------------------
-- | Run an 'Async' effect via in terms of 'A.async'.
--
--
-- @since 0.5.0.0
lowerAsync
    :: Member (Embed IO) r
    => (forall x. Sem r x -> IO x)
       -- ^ Strategy for lowering a 'Sem' action down to 'IO'. This is likely
       -- some combination of 'runM' and other interpreters composed via '.@'.
    -> Sem (Async ': r) a
    -> Sem r a
lowerAsync lower m = interpretH
    ( \case
        Async a -> do
          ma  <- runT a
          ins <- getInspectorT
          fa  <- embed $ A.async $ lower $ lowerAsync lower ma
          pureT $ fmap (inspect ins) fa

        Await a -> pureT =<< embed (A.wait a)
    )  m
{-# INLINE lowerAsync #-}

