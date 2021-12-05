module CLI where

import Data.Semigroup ((<>))
import Options.Applicative

data CLIOptions = CLIOptions
  { logging :: Int,
    configFile :: String
  }

cliParser :: Parser CLIOptions
cliParser =
  CLIOptions
    <$> option
      auto
      ( long "logging"
          <> metavar "LOG LEVEL"
          <> showDefault
          <> value 1
          <> help "Sets the logging level, choose between {0 :: VERBOSE, 1 :: INFO, 2 :: WARNING, 3 :: ERROR }"
      )
    <*> strOption
      ( long "config"
          <> metavar "CONFIG FILE"
          <> help "Location of server and peer information"
      )
