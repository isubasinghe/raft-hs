module Main where

import API
import CLI
import Options.Applicative

main :: IO ()
main = do
  -- options <- execParser opts
  runApp
  where
    opts =
      info
        (cliParser <**> helper)
        ( fullDesc
            <> progDesc "A simple key value store in Haskell implemented via acid state with fail over implemented via Raft"
            <> header "RaftKVS"
        )
