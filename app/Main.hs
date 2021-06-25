module Main where
import Options.Applicative
import CLI

main :: IO ()
main = do
    options <- execParser opts
    putStrLn ""
    where
        opts = info (cliParser <**> helper)
                ( fullDesc 
                  <> progDesc "A simple key value store in Haskell implemented via acid state with fail over implemented via Raft"
                  <> header "RaftKVS")
