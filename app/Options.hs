module Options where

import           Foundation
import           Options.Applicative

data Options = Options {
  optionsCommand :: Command,
  optionsHost    :: String,
  optionsPort    :: Int
}

data Command =
  Version

options ∷ Parser Options
options = Options <$> commandOptions <*> host <*> port

host ∷ Parser String
host = strOption (long "host" <> metavar "HOST" <> help "Tezos node RPC hostname" <> showDefault <> value "127.0.0.1")

port ∷ Parser Int
port = option auto (long "port" <> metavar "PORT" <> help "Tezos node RPC port" <> showDefault <> value 8732)

commandOptions ∷ Parser Command
commandOptions = subparser (
  command "version" (info versionOptions (progDesc "Display program version information"))
  )

versionOptions ∷ Parser Command
versionOptions = pure Version
