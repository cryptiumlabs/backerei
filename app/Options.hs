module Options where

import qualified Data.Text           as T
import           Foundation
import           Options.Applicative
import qualified Prelude             as P

data Context = Context {
  contextHomeDirectory :: P.FilePath
}

data Options = Options {
  optionsConfigPath :: P.FilePath,
  optionsCommand    :: Command,
  optionsHost       :: String,
  optionsPort       :: Int
}

data Command =
  Version |
  Init T.Text |
  Delegators |
  Expected |
  Payout

options ∷ Context → Parser Options
options ctx = Options <$> configOptions ctx <*> commandOptions <*> host <*> port

configOptions ∷ Context → Parser P.FilePath
configOptions ctx = strOption (long "config" <> metavar "FILE" <> help "Path to YAML configuration file" <> showDefault <> value (contextHomeDirectory ctx <> "/.backerei.yaml"))

host ∷ Parser String
host = strOption (long "host" <> metavar "HOST" <> help "Tezos node RPC hostname" <> showDefault <> value "127.0.0.1")

port ∷ Parser Int
port = option auto (long "port" <> metavar "PORT" <> help "Tezos node RPC port" <> showDefault <> value 8732)

commandOptions ∷ Parser Command
commandOptions = subparser (
  command "version" (info versionOptions (progDesc "Display program version information")) <>
  command "init" (info initOptions (progDesc "Initialize configuration file")) <>
  command "delegators" (info delegatorsOptions (progDesc "Display delegators")) <>
  command "expected" (info expectedOptions (progDesc "Display expected baking & endorsing rights")) <>
  command "payout" (info payoutOptions (progDesc "Calculate payouts"))
  )

versionOptions ∷ Parser Command
versionOptions = pure Version

initOptions ∷ Parser Command
initOptions = Init <$> addrOptions

addrOptions ∷ Parser T.Text
addrOptions = T.pack <$> strOption (long "address" <> metavar "tz1" <> help "tz1 address of baker implicit account")

delegatorsOptions ∷ Parser Command
delegatorsOptions = pure Delegators

expectedOptions ∷ Parser Command
expectedOptions = pure Expected

payoutOptions ∷ Parser Command
payoutOptions = pure Payout
