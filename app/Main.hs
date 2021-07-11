{-# LANGUAGE OverloadedStrings #-}
module Main where
import qualified Data.Text.IO as TIO
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
      