{-# LANGUAGE OverloadedStrings #-}

module Main where

import API
import CLI
import Data.Foldable (traverse_)
import qualified Data.Text.IO as TIO
import qualified NetworkManager as NM
import Options.Applicative
import qualified Toml

main :: IO ()
main = do
  options <- execParser opts
  config <- configText options
  case config of
    Right r -> runApp r
    Left l -> TIO.putStrLn $ Toml.prettyTomlDecodeErrors l
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
