{-# LANGUAGE LambdaCase  #-}
{-# LANGUAGE Trustworthy #-}
module Network.Tox.DHT.ClientListSpec where

import           Test.Hspec
import           Test.QuickCheck

import           Control.Monad                 (when)
import           Data.List                     (sort, sortBy)
import qualified Data.Map                      as Map
import           Data.Ord                      (comparing)
import           Data.Proxy                    (Proxy (..))
import           Network.Tox.Crypto.Key        (PublicKey)
import           Network.Tox.DHT.ClientList    (ClientList)
import qualified Network.Tox.DHT.ClientList    as ClientList
import qualified Network.Tox.DHT.Distance      as Distance
import           Network.Tox.EncodingSpec
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo


spec :: Spec
spec = do
  readShowSpec (Proxy :: Proxy ClientList)

  it "has no more than maxSize elements" $
    property $ \clientList ->
        Map.size (ClientList.nodes clientList) `shouldSatisfy` (<= ClientList.maxSize clientList)

  it "removing a node twice has no effect" $
    property $ \baseKey nodeInfo size ->
      let
        empty        = ClientList.empty baseKey
        afterAdd     = ClientList.addNode nodeInfo $ empty size
        afterRemove0 = ClientList.removeNode (NodeInfo.publicKey nodeInfo) afterAdd
        afterRemove1 = ClientList.removeNode (NodeInfo.publicKey nodeInfo) afterRemove0
      in
      afterRemove0 `shouldBe` afterRemove1

  it "adding a node twice has no effect" $
    property $ \baseKey nodeInfo size ->
      let
        empty        = ClientList.empty baseKey
        afterAdd0    = ClientList.addNode nodeInfo $ empty size
        afterAdd1    = ClientList.addNode nodeInfo afterAdd0
      in
      afterAdd0 `shouldBe` afterAdd1

  describe "addNode" $
    it "keeps the k nodes closest to the base key" $
      property $ \clientList nodeInfo ->
        let
          allNodes   = (nodeInfo:) $ Map.elems $ ClientList.nodes clientList
          keptNodes  = Map.elems $ ClientList.nodes $ ClientList.addNode nodeInfo clientList
          nodeDistance node = Distance.xorDistance (ClientList.baseKey clientList) (NodeInfo.publicKey node)
          sortNodes = sortBy $ comparing nodeDistance
        in
          take (ClientList.maxSize clientList) (sortNodes allNodes) `shouldBe` sortNodes keptNodes

  describe "foldNodes" $
    it "iterates over nodes in order of distance from the base key" $
      property $ \clientList ->
        let
          nodes             = ClientList.foldNodes (\ns n -> ns++[n]) [] clientList
          nodeDistance node = Distance.xorDistance (ClientList.baseKey clientList) (NodeInfo.publicKey node)
          sortNodes = sortBy (comparing nodeDistance)
        in
          nodes `shouldBe` sortNodes nodes
