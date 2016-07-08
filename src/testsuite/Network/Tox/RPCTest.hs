{-# LANGUAGE LambdaCase #-}
module Network.Tox.RPCTest where

import           Control.Exception          (catch)
import           Control.Monad.IO.Class     (liftIO)
import qualified Data.Text                  as Text
import           Test.Hspec

import           Data.MessagePack           (Object (..))
import           Network.MessagePack.Client (Client, RpcError (..))
import           Network.Tox.RPC            (runClient)


runTest :: Client a -> IO ()
runTest c =
  runClient c `catch` \case
    ServerError (ObjectStr msg) | msg == Text.pack "Pending" -> pending
    e -> expectationFailure $ show e


equiv1 :: (Eq r, Show r)
       => (a -> r)
       -> (a -> Client r)
       -> a -> Client ()
equiv1 f1 f2 a = do
  actual <- f2 a
  let expected = f1 a
  liftIO $ actual `shouldBe` expected


equiv2 :: (Eq r, Show r)
       => (a -> b -> r)
       -> (a -> b -> Client r)
       -> a -> b -> Client ()
equiv2 f1 f2 a b = do
  actual <- f2 a b
  let expected = f1 a b
  liftIO $ actual `shouldBe` expected


equiv3 :: (Eq r, Show r)
       => (a -> b -> c -> r)
       -> (a -> b -> c -> Client r)
       -> a -> b -> c -> Client ()
equiv3 f1 f2 a b c = do
  actual <- f2 a b c
  let expected = f1 a b c
  liftIO $ actual `shouldBe` expected


equiv4 :: (Eq r, Show r)
       => (a -> b -> c -> d -> r)
       -> (a -> b -> c -> d -> Client r)
       -> a -> b -> c -> d -> Client ()
equiv4 f1 f2 a b c d = do
  actual <- f2 a b c d
  let expected = f1 a b c d
  liftIO $ actual `shouldBe` expected
