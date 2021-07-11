{-# LANGUAGE OverloadedStrings #-}
module Main where

import API
import CLI
import Options.Applicative
import Data.Foldable(traverse_)

import qualified Toml
import qualified NetworkManager as NM
main :: IO ()
main = do
  options <- execParser opts
  config <- configText options
  case config of 
    Right r -> runApp r
    Left l -> traverse_ print l
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "A simple key value store in Haskell implemented via acid state with fail over implemented via Raft"
            <> header "RaftKVS"
        )
    configText options = do
      let fileName = configFile options
      Toml.decodeFileEither NM.configCodec fileName
      