{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE BlockArguments #-}

module NetworkManager where
import Control.Concurrent ( forkIO, threadDelay )
import Control.Concurrent.STM.TQueue ( newTQueueIO, TQueue, readTQueue )
import Network.Socket
import Network.Socket.ByteString (sendAll)
import Data.Text(Text, unpack)
import Toml (TomlCodec, (.=))
import qualified Data.Text.IO as TIO-- add 'TomlBiMap' and 'Key' here optionally
import qualified Toml
import qualified Control.Exception as E
import qualified Data.ByteString.Char8 as C
import Control.Monad.State.Lazy (forever)
import Control.Monad.Cont (forever)
import GHC.Conc (atomically)

data Peer = Peer 
    {  host       :: Host 
    ,  port       :: Port
    ,  peerID     :: Int
    }
    deriving(Show, Eq)

data Config = Config 
    {  serverPort :: Port
    ,  serverHost :: Host
    ,  serverID   :: Int
    ,  peers      :: [Peer]
    }
    deriving(Show, Eq)

newtype Port = Port Int 
    deriving(Show, Eq)

newtype Host = Host Text
    deriving(Eq)

instance Show Host where
    show h = case h of
        Host t -> unpack t

peerCodec :: TomlCodec Peer
peerCodec = Peer
    <$> Toml.diwrap (Toml.text "host") .= host
    <*> Toml.diwrap (Toml.int  "port") .= port
    <*> Toml.int  "id"   .= peerID



configCodec :: TomlCodec Config
configCodec = Config 
    <$> Toml.diwrap (Toml.int  "server.port")  .= serverPort
    <*> Toml.diwrap (Toml.text "server.host")  .= serverHost 
    <*> Toml.int               "id"            .= serverID
    <*> Toml.list peerCodec    "peer"          .= peers



sampleConfig :: Config 
sampleConfig = Config 
    { serverPort = Port 3333
    , serverHost = Host "127.0.0.1"
    , serverID = 0
    , peers = [
        Peer 
        {  host = Host "127.0.0.1"
        ,  port = Port 3334
        ,  peerID = 1
        },
        Peer 
        {  host = Host "127.0.0.1"
        ,  port = Port 3335
        ,  peerID = 2
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
            let hints = defaultHints { addrSocketType  = Stream }
            head <$> getAddrInfo (Just hints) (Just myhost) (Just $ show myport) 
        open addr = do
            sock <- socket (addrFamily addr) (addrSocketType addr) (addrProtocol addr)
            connect sock $ addrAddress addr
            pure sock
        fn c s = do
            forever $ do
                d <- atomically $ readTQueue c
                sendAll s "hello"
                pure ()
            pure ()

connectClient :: Peer -> IO (TQueue Int)
connectClient p = do
    queue <- newTQueueIO
    forkIO $ connectClient' p queue
    pure queue


startService :: Config -> IO ()
startService c = do
    let p = peers c
    chs <- traverse connectClient p
    pure () 