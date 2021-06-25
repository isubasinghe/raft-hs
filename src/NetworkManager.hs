{-# OPTIONS -Wno-unused-top-binds #-}
{-# LANGUAGE LambdaCase        #-}
{-# LANGUAGE OverloadedStrings #-}

module NetworkManager where
import Control.Applicative
import Data.Text(Text)
import Toml (TomlCodec, (.=))
import qualified Data.Text.IO as TIO
import Toml (TomlCodec, (.=))  -- add 'TomlBiMap' and 'Key' here optionally
import qualified Toml

data Peer = Peer 
    {  host       :: Host 
    ,  port       :: Port
    ,  peerID   :: Int
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
    deriving(Show, Eq)


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
        ,  peerID = 2
        },
        Peer 
        {  host = Host "127.0.0.1"
        ,  port = Port 3335
        ,  peerID = 3
        }
    ] 
    }
