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
  optionsCommand    :: Command
}

data Command =
  Version |
  Init T.Text T.Text Int T.Text Rational T.Text T.Text Int |
  Status |
  Monitor |
  Payout Bool

options ∷ Context → Parser Options
options ctx = Options <$> configOptions ctx <*> commandOptions ctx

configOptions ∷ Context → Parser P.FilePath
configOptions ctx = strOption (long "config" <> metavar "FILE" <> help "Path to YAML configuration file" <> showDefault <> value (contextHomeDirectory ctx <> "/.backerei.yaml"))

commandOptions ∷ Context -> Parser Command
commandOptions ctx = subparser (
  command "version" (info versionOptions (progDesc "Display program version information")) <>
  command "init" (info (initOptions ctx) (progDesc "Initialize configuration file")) <>
  command "status" (info statusOptions (progDesc "Display delegate status")) <>
  command "monitor" (info monitorOptions (progDesc "Monitor baking & endorsing status")) <>
  command "payout" (info payoutOptions (progDesc "Calculate payouts"))
  )

versionOptions ∷ Parser Command
versionOptions = pure Version

initOptions ∷ Context -> Parser Command
initOptions ctx = Init <$> addrOptions <*> hostOptions <*> portOptions <*> fromOptions <*> feeOptions <*> dbPathOptions ctx <*> clientPathOptions <*> startingCycleOptions

addrOptions ∷ Parser T.Text
addrOptions = T.pack <$> strOption (long "tz1" <> metavar "tz1" <> help "tz1 address of baker implicit account")

hostOptions ∷ Parser T.Text
hostOptions = T.pack <$> strOption (long "host" <> metavar "HOST" <> help "Tezos node RPC hostname" <> showDefault <> value "127.0.0.1")

portOptions ∷ Parser Int
portOptions = option auto (long "port" <> metavar "PORT" <> help "Tezos node RPC port" <> showDefault <> value 8732)

fromOptions :: Parser T.Text
fromOptions = T.pack <$> strOption (long "from" <> metavar "FROM" <> help "Address to send payouts from")

feeOptions :: Parser Rational
feeOptions = option auto (long "fee" <> metavar "FEE" <> help "Fractional fee taken by baker" <> showDefault <> value (1 / 10))

dbPathOptions :: Context -> Parser T.Text
dbPathOptions ctx = T.pack <$> strOption (long "database-path" <> metavar "DBPATH" <> help "Path to JSON DB" <> showDefault <> value (contextHomeDirectory ctx <> "/.backerei.json"))

clientPathOptions :: Parser T.Text
clientPathOptions = T.pack <$> strOption (long "path" <> metavar "PATH" <> help "Path to 'tezos-client' executable" <> showDefault <> value "/usr/local/bin/tezos-client")

startingCycleOptions :: Parser Int
startingCycleOptions = option auto (long "starting-cycle" <> metavar "CYCLE" <> help "Cycle at which baker became a delegate")

statusOptions ∷ Parser Command
statusOptions = pure Status

monitorOptions ∷ Parser Command
monitorOptions = pure Monitor

payoutOptions ∷ Parser Command
payoutOptions = Payout <$> noDryRunOptions

noDryRunOptions :: Parser Bool
noDryRunOptions = switch (long "no-dry-run" <> help "Really transfer Tezzies")
