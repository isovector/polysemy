{-# LANGUAGE TemplateHaskell #-}

module Polysemy.Trace
  ( -- * Effect
    Trace (..)

    -- * Actions
  , trace

    -- * Interpretations
  , traceToIO
  , runTraceList
  , ignoreTrace
  , traceToOutput

    -- * Interpretations for Other Effects
  , outputToTrace
  ) where

import Polysemy
import Polysemy.Output


------------------------------------------------------------------------------
-- | An effect for logging strings.
data Trace m a where
  Trace :: String -> Trace m ()

makeSem ''Trace


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by printing the messages to stdout.
traceToIO :: Member (Embed IO) r => Sem (Trace ': r) a -> Sem r a
traceToIO = interpret $ \case
  Trace m -> embed $ putStrLn m
{-# INLINE traceToIO #-}


------------------------------------------------------------------------------
-- | Run a 'Trace' effect by ignoring all of its messages.
ignoreTrace :: Sem (Trace ': r) a -> Sem r a
ignoreTrace = interpret $ \case
  Trace _ -> pure ()
{-# INLINE ignoreTrace #-}


------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
traceToOutput
    :: Member (Output String) r
    => Sem (Trace ': r) a
    -> Sem r a
traceToOutput = interpret $ \case
  Trace m -> output m
{-# INLINE traceToOutput #-}


------------------------------------------------------------------------------
-- | Get the result of a 'Trace' effect as a list of 'String's.
--
-- @since 0.5.0.0
runTraceList
    :: Sem (Trace ': r) a
    -> Sem r ([String], a)
runTraceList = runOutputList . reinterpret (
  \case
    Trace m -> output m
  )
{-# INLINE runTraceList #-}


------------------------------------------------------------------------------
-- | Transform a 'Trace' effect into a 'Output' 'String' effect.
--
-- @since 0.1.2.0
outputToTrace
    :: ( Show w
       , Member Trace r
       )
    => Sem (Output w ': r) a
    -> Sem r a
outputToTrace = interpret $ \case
  Output m -> trace $ show m
{-# INLINE outputToTrace #-}

