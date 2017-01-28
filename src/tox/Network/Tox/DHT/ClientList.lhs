\section{Client Lists}

\begin{code}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE Safe           #-}
module Network.Tox.DHT.ClientList where

import           Control.Applicative           ((<$>), (<*>))
import           Control.Monad                 (join)
import           Control.Monad.Trans.Writer    (Writer, tell)
import           Data.Map                      (Map)
import qualified Data.Map                      as Map
import           Test.QuickCheck.Arbitrary     (Arbitrary, arbitrary,
                                                arbitrarySizedNatural)
import           Test.QuickCheck.Gen           (Gen)
import qualified Test.QuickCheck.Gen           as Gen

import           Network.Tox.Crypto.Key        (PublicKey)
import qualified Network.Tox.DHT.Distance      as Distance
import           Network.Tox.NodeInfo.NodeInfo (NodeInfo)
import qualified Network.Tox.NodeInfo.NodeInfo as NodeInfo
import           Network.Tox.Time              (TimeDiff, TimeStamp)
import qualified Network.Tox.Time              as Time


{-------------------------------------------------------------------------------
 -
 - :: Implementation.
 -
 ------------------------------------------------------------------------------}

\end{code}

A Client List of \textit{maximum size} \texttt{k} with a given public key as
\textit{base key} is an ordered set of at most \texttt{k} nodes close to the
base key.  The elements are sorted by \href{#distance}{distance} from the base
key.  Thus, the first (smallest) element of the set is the closest one to the
base key in that set, the last (greatest) element is the furthest away.  The
maximum size and base key are constant throughout the lifetime of a Client
List.


\begin{code}

data ClientList = ClientList
  { baseKey :: PublicKey
  , maxSize :: Int
  , nodes   :: ClientNodes
  }
  deriving (Eq, Read, Show)

type ClientNodes = Map Distance.Distance ClientNode

data ClientNode = ClientNode
  { nodeInfo  :: NodeInfo
  , lastPing  :: TimeStamp
  , pingCount :: Int
  }
  deriving (Eq, Read, Show)

newNode :: TimeStamp -> NodeInfo -> ClientNode
newNode time node = ClientNode node time 0

nodeInfos :: ClientList -> [NodeInfo]
nodeInfos = map nodeInfo . Map.elems . nodes

empty :: PublicKey -> Int -> ClientList
empty publicKey size = ClientList
  { baseKey = publicKey
  , maxSize = size
  , nodes   = Map.empty
  }

isEmpty :: ClientList -> Bool
isEmpty = Map.null . nodes

updateClientNodes :: (ClientNodes -> ClientNodes) -> ClientList -> ClientList
updateClientNodes f clientList@ClientList{ nodes } =
  clientList{nodes = f nodes}

lookup :: PublicKey -> ClientList -> Maybe NodeInfo
lookup publicKey _cl@ClientList{ baseKey, nodes } =
  nodeInfo <$> Distance.xorDistance publicKey baseKey `Map.lookup` nodes

\end{code}


A Client List is \textit{full} when the number of nodes it contains is the
maximum size of the list.

A node is \textit{viable} for entry if the Client List is not \textit{full} or the
node's public key has a lower distance from the base key than the current entry
with the greatest distance.

If a node is \textit{viable} and the Client List is \textit{full}, the entry
with the greatest distance from the base key is removed to keep the size below
the maximum configured size.

Adding a node whose key already exists will result in an update of the Node
Info in the Client List.  Removing a node for which no Node Info exists in the
Client List has no effect.  Thus, removing a node twice is permitted and has the
same effect as removing it once.

\begin{code}

addNode :: TimeStamp -> NodeInfo -> ClientList -> ClientList
addNode time nodeInfo clientList@ClientList{ baseKey, maxSize } =
  (`updateClientNodes` clientList) $
    mapTake maxSize
    . Map.insert
      (Distance.xorDistance (NodeInfo.publicKey nodeInfo) baseKey)
      (newNode time nodeInfo)

removeNode :: PublicKey -> ClientList -> ClientList
removeNode publicKey clientList =
  (`updateClientNodes` clientList) $
    Map.delete $ Distance.xorDistance publicKey $ baseKey clientList

-- | 'mapTake' is 'Data.Map.take' in >=containers-0.5.8, but we define it for
-- compatibility with older versions.
mapTake :: Int -> Map k a -> Map k a
mapTake n = Map.fromDistinctAscList . take n . Map.toAscList

\end{code}

The iteration order of a Client List is in order of distance from the base
key.  I.e. the first node seen in iteration is the closest, and the last node
is the furthest away in terms of the distance metric.

\begin{code}

foldNodes :: (a -> NodeInfo -> a) -> a -> ClientList -> a
foldNodes f x = foldl f x . nodeInfos


pingInterval :: TimeDiff
pingInterval = Time.seconds 60

maxPings :: Int
maxPings = 2

pingNodes :: TimeStamp -> ClientList -> Writer [(NodeInfo, PublicKey)] ClientList
pingNodes time clientList@ClientList{ nodes } =
  (\x -> clientList{ nodes = x }) <$> traverseMaybe pingNode nodes
  where
    traverseMaybe :: Applicative f =>
        (a -> f (Maybe b)) -> Map.Map k a -> f (Map.Map k b)
    traverseMaybe f = (Map.mapMaybe id <$>) . traverse f
    pingNode :: ClientNode -> Writer [(NodeInfo, PublicKey)] (Maybe ClientNode)
    pingNode clientNode@ClientNode{ nodeInfo, lastPing, pingCount } =
      if time - lastPing < pingInterval
      then pure $ Just clientNode
      else (tell [requestInfo] *>) . pure $
        if pingCount + 1 < maxPings
        then Just $ clientNode{ lastPing = time, pingCount = pingCount + 1 }
        else Nothing
      where requestInfo = (nodeInfo, baseKey clientList)

{-------------------------------------------------------------------------------
 -
 - :: Tests.
 -
 ------------------------------------------------------------------------------}


genClientList :: PublicKey -> Int -> Gen ClientList
genClientList publicKey size =
  foldl (flip $ uncurry addNode) (empty publicKey size) <$> Gen.listOf arbitrary


instance Arbitrary ClientList where
  arbitrary = join $ genClientList <$> arbitrary <*> arbitrarySizedNatural

instance Arbitrary ClientNode where
  arbitrary = ClientNode <$> arbitrary <*> arbitrary <*> arbitrary
\end{code}
