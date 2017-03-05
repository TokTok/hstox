\begin{code}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE Trustworthy                #-}

-- | abstraction layer for network functionality.
-- The intention is to
--   (i) separate the logic of the protocol from its binary encoding, and
--   (ii) allow a simulated network in place of actual network IO.
module Network.Tox.Network.Networked where

import           Control.Monad                        (guard, replicateM, void)
import           Control.Monad.IO.Class               (liftIO)
import           Control.Monad.Random                 (RandT)
import           Control.Monad.Reader                 (ReaderT, ask, runReaderT)
import           Control.Monad.State                  (MonadState, StateT)
import           Control.Monad.Trans.Class            (lift)
import           Control.Monad.Trans.Maybe            (MaybeT (..), runMaybeT)
import           Control.Monad.Writer                 (WriterT, tell, runWriterT, execWriterT)
import           Data.Binary                          (Binary)
import           Data.Map                             (Map)
import qualified Data.Map                             as Map
import           System.Random                        (Random, StdGen, randomIO)

import           Network.Tox.DHT.DhtState             (DhtState)
import           Network.Tox.Network.MonadRandomBytes (MonadRandomBytes)
import           Network.Tox.NodeInfo.NodeInfo        (NodeInfo)
import           Network.Tox.Protocol.Packet          (Packet (..))
import           Network.Tox.Timed                    (Timed, TimedT)

class Monad m => Networked m where
  sendPacket :: (Binary payload, Show payload) => NodeInfo -> Packet payload -> m ()
-- | actual network IO
instance Networked (StateT NetworkState IO) where
  -- | TODO
  sendPacket _ _ = return ()

-- | TODO: sockets etc
type NetworkState = ()

type NetworkEvent = String
newtype NetworkLogged m a = NetworkLogged (WriterT [NetworkEvent] m a)
  deriving (Monad, Functor, Applicative, MonadState s, MonadRandomBytes, Timed)

runNetworkLogged :: Monad m => NetworkLogged m a -> m (a, [NetworkEvent])
runNetworkLogged (NetworkLogged m) = runWriterT m
evalNetworkLogged :: Monad m => NetworkLogged m a -> m a
evalNetworkLogged = (fst <$>) . runNetworkLogged
execNetworkLogged :: Monad m => NetworkLogged m a -> m [NetworkEvent]
execNetworkLogged (NetworkLogged m) = execWriterT m

-- | just log network events
instance Monad m => Networked (NetworkLogged m) where
  sendPacket to packet = NetworkLogged $
    tell [">>> " ++ show to ++ " : " ++ show packet]

instance Networked m => Networked (ReaderT r m) where
  sendPacket = (lift .) . sendPacket
instance (Monoid w, Networked m) => Networked (WriterT w m) where
  sendPacket = (lift .) . sendPacket
instance Networked m => Networked (RandT s m) where
  sendPacket = (lift .) . sendPacket
instance Networked m => Networked (StateT s m) where
  sendPacket = (lift .) . sendPacket
instance Networked m => Networked (TimedT m) where
  sendPacket = (lift .) . sendPacket

\end{code}
