{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.RpcPacketSpec where

import           Test.Hspec

import           Data.Proxy                (Proxy (..))
import           Network.Tox.DHT.RpcPacket (RpcPacket)
import           Network.Tox.EncodingSpec


spec :: Spec
spec = do
  rpcSpec (Proxy :: Proxy (RpcPacket Int))
  binarySpec (Proxy :: Proxy (RpcPacket Int))
  readShowSpec (Proxy :: Proxy (RpcPacket Int))
