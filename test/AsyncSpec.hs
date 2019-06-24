{-# LANGUAGE NumDecimals #-}

module AsyncSpec where

import Control.Concurrent
import Control.Monad
import Polysemy
import Polysemy.Async
import Polysemy.State
import Polysemy.Trace
import Test.Hspec


spec :: Spec
spec = describe "async" $ do
  it "should thread state and not lock" $ do
    (ts, (s, r)) <- runM
                  . runTraceAsList
                  . runState "hello"
                  . runAsync $ do
      let message :: Member Trace r => Int -> String -> Sem r ()
          message n msg = trace $ mconcat
            [ show n, "> ", msg ]

      a1 <- async $ do
          v <- get @String
          message 1 v
          put $ reverse v

          sendM $ threadDelay 1e5
          get >>= message 1

          sendM $ threadDelay 1e5
          get @String

      void $ async $ do
          sendM $ threadDelay 5e4
          get >>= message 2
          put "pong"

      await a1 <* put "final"

    ts `shouldContain` ["1> hello", "2> olleh", "1> pong"]
    s `shouldBe` "final"
    r `shouldBe` Just "pong"
