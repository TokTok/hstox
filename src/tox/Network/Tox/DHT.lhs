\chapter{DHT}

\begin{code}
{-# LANGUAGE Safe #-}
module Network.Tox.DHT where
\end{code}

The DHT is a self-organizing swarm of all nodes in the Tox network.  A node in
the Tox network is also called a "Tox node".  When we talk about "peers", we mean
any node that is not the local node (the subject).  This module takes care of
finding the IP and port of nodes and establishing a route to them directly via
UDP using \href{#hole-punching}{hole punching} if necessary.  The DHT only runs
on UDP and so is only used if UDP works.

Every node in the Tox DHT has an ephemeral Key Pair called the DHT Key Pair,
consisting of the DHT Secret Key and the DHT Public Key.  The DHT Public Key
acts as the node address.  The DHT Key Pair is renewed every time the Tox
instance is closed or restarted.  An implementation may choose to renew the key
more often, but doing so will disconnect all peers.

The DHT public key of a friend is found using the \href{#onion}{onion} module.
Once the DHT public key of a friend is known, the DHT is used to find them and
connect directly to them via UDP.

\input{src/tox/Network/Tox/DHT/Distance.lhs}
\input{src/tox/Network/Tox/DHT/KBuckets.lhs}
\input{src/tox/Network/Tox/DHT/DhtState.lhs}

\section{Self-organisation}

Self-organising in the DHT occurs through each DHT peer connecting to an
arbitrary number of peers closest to their own DHT public key and some that are
further away.

If each peer in the network knows the peers with the DHT public key closest to
its DHT public key, then to find a specific peer with public key X a peer just
needs to recursively ask peers in the DHT for known peers that have the DHT
public keys closest to X.  Eventually the peer will find the peers in the DHT
that are the closest to that peer and, if that peer is online, they will find
them.

\input{src/tox/Network/Tox/DHT/DhtPacket.lhs}

\section{RPC Services}

\input{src/tox/Network/Tox/DHT/RpcPacket.lhs}
\input{src/tox/Network/Tox/DHT/PingPacket.lhs}

\subsection{Nodes Service}

The Nodes Service is used to query another DHT node for up to 4 nodes they know
that are the closest to a requested node.

The DHT Nodes RPC service uses the Packed Node Format.

Only the UDP Protocol (IP Type \texttt{2} and \texttt{10}) is used in the DHT
module when sending nodes with the packed node format.  This is because the TCP
Protocol is used to send TCP relay information and the DHT is UDP only.

\input{src/tox/Network/Tox/DHT/NodesRequest.lhs}
\input{src/tox/Network/Tox/DHT/NodesResponse.lhs}

\subsection{Periodic sending of Nodes Requests}

For each Nodes List in the DHT State, every 20 seconds a Nodes Request is sent
to a random node on the list, searching for the base key of the list.

Random nodes are chosen since being able to predict which node a node will
send a request to next could make some attacks that disrupt the network
easier, as it adds a possible attack vector.

Furthermore, for each Nodes List in the DHT State, each node on the list is
sent a Nodes Request every 60 seconds, searching for the base key of the list.

(c-toxcore's implementation: a Last Pinged time is maintained for each
node in each list. When a node is added to a list, if doing so evicts a node
from the list then the Last Pinged time is set to that of the evicted node,
and otherwise it is set to 0.)

Nodes from which we consistently fail to receive Nodes Responses should be
removed from the DHT State.

(c-toxcore's implementation: nodes from which we have not received a Nodes
Response for 122 seconds are considered Bad; they remain in the DHT State, but
are preferentially overwritten when adding to the DHT State, and are ignored
for all operations except the once-per-60s pinging described above. If we have
not received a Nodes Response for 182 seconds, the node is not even pinged. So
one ping is sent after the node becomes Bad. In the special case that every
node in the Close List is Bad, they are all pinged once more.)

Proposed hs-toxcore implementation: store on the Client Lists for each node a
Last Pinged timestamp and a Pings Counter.  Nodes are added with these
set to the current time and 0, respectively.  This includes updating an
already present node.  Periodically pass through the DHT State nodes, and for
each which is due a ping: ping it, update the timestamp, and increment the
counter, and if the counter is then 2 (configurable constant), remove the node
from the list. This is pretty close to the behaviour of c-toxcore, but much
simpler.

\subsection{Handling Nodes Response packets}
When a valid Nodes Response packet is received, it is first checked that a
Nodes Request was sent within the last 60 seconds to the node from which the
response was received; if not, the packet is ignored. Otherwise, firstly the
node from which the response was sent it is added to the state; see the
k-Buckets and Client List specs for details on this operation. Secondly, for
each node listed in the response and for each Nodes List in the DHT State to
which the node is viable for entry, a Nodes Request is sent to the node with
the requested public key being the base key of the Nodes List.

An implementation may choose not to send every such Nodes Request.
(c-toxcore only sends only so many per list (8 for the Close List, 4 for a
Search Entry) per call to Do_DHT(), prioritising the closest to the base key).

\subsection{Handling Nodes Request packets}
On receiving a Nodes Request packet, the 4 nodes in the DHT State which are
closest to the public key in the packet are found, and sent back to the node
which sent the request in a Nodes Response packet. If there are fewer than 4
nodes in the state, just those nodes are sent.

\subsection{Effects of chosen constants on performance}
If the bucket size of the k-buckets were increased, it would increase the
amount of packets needed to check if each node is still alive, which would
increase the bandwidth usage, but reliability would go up.  If the number of
nodes were decreased, reliability would go down along with bandwidth usage.
The reason for this relationship between reliability and number of nodes is
that if we assume that not every node has its UDP ports open or is behind a
cone NAT it means that each of these nodes must be able to store a certain
number of nodes behind restrictive NATs in order for others to be able to find
those nodes behind restrictive NATs.  For example if 7/8 nodes were behind
restrictive NATs, using 8 nodes would not be enough because the chances of
some of these nodes being impossible to find in the network would be too high.

TODO(zugz): this seems a rather wasteful solution to this problem.

If the ping timeouts and delays between pings were higher it would decrease the
bandwidth usage but increase the amount of disconnected nodes that are still
being stored in the lists.  Decreasing these delays would do the opposite.

If the maximum size 8 of the DHT Search Entry Client Lists were increased
would increase the bandwidth usage, might increase hole punching efficiency on
symmetric NATs (more ports to guess from, see Hole punching) and might increase
the reliability.  Lowering this number would have the opposite effect.

The timeouts and number of nodes in lists for toxcore were picked by feeling
alone and are probably not the best values.  This also applies to the behavior
which is simple and should be improved in order to make the network resist
better to sybil attacks.

TODO: consider giving min and max values for the constants.

\section{DHT Request packets}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8_t} (0x20) \\
  \texttt{32}        & receiver's DHT public key \\
  \texttt{32}        & sender's DHT public key \\
  \texttt{24}        & nonce \\
  \texttt{?}         & encrypted payload \\
\end{tabular}

DHT Request packets are packets that can be sent across one DHT node to one
that they know.  They are used to send encrypted data to friends that we are
not necessarily connected to directly in the DHT.

A DHT node that receives a DHT request packet will check whether the receiver's
public key is their DHT public key and, if it is, they will decrypt and handle
the packet.  If it is not they will check whether they know that DHT public key
(if it's in their list of close nodes).  If it isn't, they will drop the
packet.  If it is they will resend the exact packet to that DHT node.

The encrypted message is encrypted using the receiver's DHT Public key, the
sender's DHT private key and a randomly generated 24 byte nonce.

DHT request packets are used for DHT public key packets (see
\href{#onion}{onion}) and NAT ping packets.

\subsection{NAT ping packets}

A NAT ping packet is sent as the payload of a DHT request packet. 

NAT ping packets are used to see if a friend we are not connected to directly
is online and ready to do the hole punching.

\subsubsection{NAT ping request}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8_t} (0xfe) \\
  \texttt{1}         & \texttt{uint8_t} (0x00) \\
  \texttt{8}         & \texttt{uint64_t} random number \\
\end{tabular}

\subsubsection{NAT ping response}

\begin{tabular}{l|l}
  Length             & Contents \\
  \hline
  \texttt{1}         & \texttt{uint8_t} (0xfe) \\
  \texttt{1}         & \texttt{uint8_t} (0x01) \\
  \texttt{8}         & \texttt{uint64_t} random number (the same that was received in request) \\
\end{tabular}

TODO: handling these packets.

\section{NATs}

We assume that peers are either directly accessible or are behind one of 3
types of NAT:

Cone NATs: Assign one whole port to each UDP socket behind the NAT; any packet
from any IP/port sent to that assigned port from the internet will be forwarded
to the socket behind it.

Restricted Cone NATs: Assign one whole port to each UDP socket behind the NAT.
However, it will only forward packets from IPs that the UDP socket has sent a
packet to.

Symmetric NATs: The worst kind of NAT, they assign a new port for each IP/port
a packet is sent to.  They treat each new peer you send a UDP packet to as a
\texttt{'connection'} and will only forward packets from the IP/port of that
\texttt{'connection'}.


\section{Hole punching}

Holepunching on normal cone NATs is achieved simply through the way in which
the DHT functions.

If more than half of the 8 peers closest to the friend in the DHT return an
IP/port for the friend and we send a ping request to each of the returned
IP/ports but get no response.  If we have sent 4 ping requests to 4 IP/ports
that supposedly belong to the friend and get no response, then this is enough
for toxcore to start the hole punching.  The numbers 8 and 4 are used in
toxcore and were chosen based on feel alone and so may not be the best numbers.

Before starting the hole punching, the peer will send a NAT ping packet to the
friend via the peers that say they know the friend.  If a NAT ping response
with the same random number is received the hole punching will start.

If a NAT ping request is received, we will first check if it is from a friend.
If it is not from a friend it will be dropped.  If it is from a friend, a
response with the same 8 byte number as in the request will be sent back via
the nodes that know the friend sending the request.  If no nodes from the
friend are known, the packet will be dropped.

Receiving a NAT ping response therefore means that the friend is both online
and actively searching for us, as that is the only way they would know nodes
that know us.  This is important because hole punching will work only if the
friend is actively trying to connect to us.

NAT ping requests are sent every 3 seconds in toxcore, if no response is
received for 6 seconds, the hole punching will stop.  Sending them in longer
intervals might increase the possibility of the other node going offline and
ping packets sent in the hole punching being sent to a dead peer but decrease
bandwidth usage.  Decreasing the intervals will have the opposite effect.

There are 2 cases that toxcore handles for the hole punching.  The first case
is if each 4+ peers returned the same IP and port.  The second is if the 4+
peers returned same IPs but different ports.

A third case that may occur is the peers returning different IPs and ports.
This can only happen if the friend is behind a very restrictive NAT that cannot
be hole punched or if the peer recently connected to another internet
connection and some peers still have the old one stored.  Since there is
nothing we can do for the first option it is recommended to just use the most
common IP returned by the peers and to ignore the other IP/ports.

In the case where the peers return the same IP and port it means that the other
friend is on a restricted cone NAT.  These kinds of NATs can be hole punched by
getting the friend to send a packet to our public IP/port.  This means that
hole punching can be achieved easily and that we should just continue sending
DHT ping packets regularly to that IP/port until we get a ping response.  This
will work because the friend is searching for us in the DHT and will find us
and will send us a packet to our public IP/port (or try to with the hole
punching), thereby establishing a connection.

For the case where peers do not return the same ports, this means that the
other peer is on a symmetric NAT.  Some symmetric NATs open ports in sequences
so the ports returned by the other peers might be something like: 1345, 1347,
1389, 1395.  The method to hole punch these NATs is to try to guess which ports
are more likely to be used by the other peer when they try sending us ping
requests and send some ping requests to these ports.  Toxcore just tries all
the ports beside each returned port (ex: for the 4 ports previously it would
try: 1345, 1347, 1389, 1395, 1346, 1348, 1390, 1396, 1344, 1346...) getting
gradually further and further away and, although this works, the method could
be improved.  When using this method toxcore will try up to 48 ports every 3
seconds until both connect.  After 5 tries toxcore doubles this and starts
trying ports from 1024 (48 each time) along with the previous port guessing.
This is because I have noticed that this seemed to fix it for some symmetric
NATs, most likely because a lot of them restart their count at 1024.

Increasing the amount of ports tried per second would make the hole punching go
faster but might DoS NATs due to the large number of packets being sent to
different IPs in a short amount of time.  Decreasing it would make the hole
punching slower.

This works in cases where both peers have different NATs.  For example, if A
and B are trying to connect to each other: A has a symmetric NAT and B a
restricted cone NAT.  A will detect that B has a restricted cone NAT and keep
sending ping packets to his one IP/port.  B will detect that A has a symmetric
NAT and will send packets to it to try guessing his ports.  If B manages to
guess the port A is sending packets from they will connect together.

\section{DHT Bootstrap Info (0xf0)}

Bootstrap nodes are regular Tox nodes with a stable DHT public key. This means
the DHT public key does not change across restarts. DHT bootstrap nodes have one
additional request kind: Bootstrap Info. The request is simply a packet of
length 78 bytes where the first byte is 0xf0. The other bytes are ignored.

The response format is as follows:

\begin{tabular}{l|l|l}
  Length             & Type        & \href{#protocol-packet}{Contents} \\
  \hline
  \texttt{4}         & Word32      & Bootstrap node version \\
  \texttt{256}       & Bytes       & Message of the day \\
\end{tabular}

\section{DHT Initialisation}
TODO: describe behaviour at start up, including bootstrapping,
bootstrap_times, fake friends, and any other subtleties.
