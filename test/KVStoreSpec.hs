module KVStoreSpec where

import qualified Data.Map as M
import           Polysemy
import           Polysemy.KVStore
import           Test.Hspec


program :: Member (KVStore Bool Int) r => Sem r (Maybe Int)
program = do
  a <- lookupKV False
  b <- lookupKV True
  pure $ (+) <$> a <*> b


spec :: Spec
spec = do
  describe "KVStore" $ do
    let itShouldBe m v = run (fmap snd $ runKVStorePurely @Bool @Int (M.fromList m) program) `shouldBe` v

    it "should fail to find missing keys" $ do
      itShouldBe [] Nothing

    it "should add if all the keys are there" $ do
      itShouldBe [(False, 5), (True, 10)] $ Just 15

