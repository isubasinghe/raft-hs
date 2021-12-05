{-# LANGUAGE OverloadedStrings #-}

module NetworkManager where

import Control.Concurrent (forkFinally, forkIO, threadDelay)
import Control.Concurrent.STM.TQueue (TQueue, newTQueueIO, readTQueue)
import qualified Control.Exception as E
import Control.Monad.Cont (forever)
import Control.Monad.IO.Class (MonadIO)
import Control.Monad.State.Lazy (forever)
import qualified Data.ByteString.Char8 as C
import qualified Data.Map as M
import Data.Text (Text, unpack)
import qualified Data.Text.IO as TIO
import Debug.Trace
import GHC.Conc (atomically)
import Network.Socket
import Network.Socket (setCloseOnExecIfNeeded)
import Network.Socket.ByteString (recv, sendAll)
import Raft
import Toml (TomlCodec, (.=))
import qualified Toml

data Peer = Peer
  { host :: Host,
    port :: Port,
    peerID :: Int
  }
  deriving (Show, Eq)

data Config = Config
  { serverPort :: Port,
    serverHost :: Host,
    serverID :: Int,
    peers :: [Peer]
  }
  deriving (Show, Eq)

newtype Port = Port Int
  deriving (Eq)

instance Show Port where
  show p = case p of
    Port p' -> show p'

newtype Host = Host Text
  deriving (Eq)

instance Show Host where
  show h = case h of
    Host t -> unpack t

data NetworkManager = NetworkManager
  { connections :: M.Map Int (TQueue Int),
    clients :: [Socket],
    raft :: RaftState
  }

peerCodec :: TomlCodec Peer
peerCodec =
  Peer
    <$> Toml.diwrap (Toml.text "host") .= host
    <*> Toml.diwrap (Toml.int "port") .= port
    <*> Toml.int "id" .= peerID

configCodec :: TomlCodec Config
configCodec =
  Config
    <$> Toml.diwrap (Toml.int "server.port") .= serverPort
    <*> Toml.diwrap (Toml.text "server.host") .= serverHost
    <*> Toml.int "id" .= serverID
    <*> Toml.list peerCodec "peer" .= peers

sampleConfig :: Config
sampleConfig =
  Config
    { serverPort = Port 3333,
      serverHost = Host "127.0.0.1",
      serverID = 0,
      peers =
        [ Peer
            { host = Host "127.0.0.1",
              port = Port 3334,
              peerID = 1
            },
          Peer
            { host = Host "127.0.0.1",
              port = Port 3335,
              peerID = 2
            }
        ]
    }

connectClient' :: Peer -> TQueue Int -> IO ()
connectClient' p c = do
  addr <- resolve
  forever $ do
    threadDelay $ 1000 * 100
    E.bracket (open addr) close $ fn c
  where
    resolve = do
      let myhost = show $ host p
      let myport = show $ port p
      let hints = defaultHints {addrSocketType = Stream}
      head <$> getAddrInfo (Just hints) (Just myhost) (Just $ show myport)
    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      connect sock $ addrAddress addr
      pure sock
    fn c s = do
      forever $ do
        d <- atomically $ readTQueue c
        sendAll s "hello"

connectClient :: Peer -> IO (TQueue Int)
connectClient p = do
  queue <- newTQueueIO
  forkIO $ connectClient' p queue
  pure queue

startService :: Config -> IO ()
startService c = do
  -- let p = peers c
  -- chs <- traverse connectClient p
  -- let x = M.fromList $ zip (map peerID p) chs

  addr <- resolve
  E.bracket (open addr) close loop
  where
    resolve = do
      let myhost = show $ serverHost c
      let myport = show $ serverPort c
      let hints =
            defaultHints
              { addrFlags = [AI_PASSIVE],
                addrSocketType = Stream
              }
      head <$> getAddrInfo (Just hints) (Just myhost) (Just myport)

    open addr = do
      sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
      setSocketOption sock ReuseAddr 1
      withFdSocket sock setCloseOnExecIfNeeded
      bind sock $ addrAddress addr
      listen sock 1024
      pure sock

    loop sock = forever $ do
      (conn, _peer) <- accept sock
      forkFinally (fn conn) (const $ gracefulClose conn 5000)

    fn conn = do
      msg <- recv conn 1024
      traceIO $ C.unpack msg
      threadDelay $ 1000 * 100
      -- force catch client disconnect
      sendAll conn "pong"
      fn conn
