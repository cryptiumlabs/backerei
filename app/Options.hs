module Options where

import qualified Config
import qualified Data.Text           as T
import           Foundation
import           Options.Applicative
import qualified Prelude             as P

newtype Context = Context {
  contextHomeDirectory :: P.FilePath
}

data Options = Options {
  optionsConfigPath :: P.FilePath,
  optionsCommand    :: Command
}

data Command =
  Version |
  Init T.Text T.Text Int T.Text T.Text Rational T.Text T.Text T.Text Int Int Int Int Int |
  Monitor |
  Payout Bool Bool Bool

options ∷ Context → Parser Options
options ctx = Options <$> configOptions ctx <*> commandOptions ctx

configOptions ∷ Context → Parser P.FilePath
configOptions ctx = strOption (long "config" <> metavar "FILE" <> help "Path to YAML configuration file" <> showDefault <> value (contextHomeDirectory ctx <> "/.backerei.yaml"))

commandOptions ∷ Context -> Parser Command
commandOptions ctx = subparser (
  command "version" (info versionOptions (progDesc "Display program version information")) <>
  command "init" (info (initOptions ctx) (progDesc "Initialize configuration file")) <>
  command "monitor" (info monitorOptions (progDesc "Monitor baking & endorsing status")) <>
  command "payout" (info payoutOptions (progDesc "Calculate payouts"))
  )

versionOptions ∷ Parser Command
versionOptions = pure Version

initOptions ∷ Context -> Parser Command
initOptions ctx = Init <$> addrOptions <*> hostOptions <*> portOptions <*> fromOptions <*> fromNameOptions <*> feeOptions <*> dbPathOptions ctx <*> clientPathOptions <*> clientConfigFileOptions <*> startingCycleOptions <*> cycleLengthOptions <*> snapshotIntervalOptions <*> preservedCyclesOptions <*> payoutDelayOptions

addrOptions ∷ Parser T.Text
addrOptions = T.pack <$> strOption (long "tz1" <> metavar "tz1" <> help "tz1 address of baker implicit account")

hostOptions ∷ Parser T.Text
hostOptions = T.pack <$> strOption (long "host" <> metavar "HOST" <> help "Tezos node RPC hostname" <> showDefault <> value "127.0.0.1")

portOptions ∷ Parser Int
portOptions = option auto (long "port" <> metavar "PORT" <> help "Tezos node RPC port" <> showDefault <> value 8732)

fromOptions :: Parser T.Text
fromOptions = T.pack <$> strOption (long "from" <> metavar "FROM" <> help "Address to send payouts from")

fromNameOptions :: Parser T.Text
fromNameOptions = T.pack <$> strOption (long "from-name" <> metavar "NAME" <> help "Local client alias of the address to send payouts from")

feeOptions :: Parser Rational
feeOptions = option auto (long "fee" <> metavar "FEE" <> help "Fractional fee taken by baker" <> showDefault <> value Config.defaultFee)

dbPathOptions :: Context -> Parser T.Text
dbPathOptions ctx = T.pack <$> strOption (long "database-path" <> metavar "DBPATH" <> help "Path to JSON DB" <> showDefault <> value (contextHomeDirectory ctx <> "/.backerei.json"))

clientPathOptions :: Parser T.Text
clientPathOptions = T.pack <$> strOption (long "client-path" <> metavar "PATH" <> help "Path to 'tezos-client' executable" <> showDefault <> value "/usr/local/bin/tezos-client")

clientConfigFileOptions :: Parser T.Text
clientConfigFileOptions = T.pack <$> strOption (long "client-config-file" <> metavar "PATH" <> help "Path to 'tezos-client' config file")

startingCycleOptions :: Parser Int
startingCycleOptions = option auto (long "starting-cycle" <> metavar "CYCLE" <> help "Cycle at which baker became a delegate")

cycleLengthOptions :: Parser Int
cycleLengthOptions = option auto (long "cycle-length" <> metavar "BLOCKS" <> help "Length of a single cycle in blocks" <> showDefault <> value 4096)

snapshotIntervalOptions :: Parser Int
snapshotIntervalOptions = option auto (long "snapshot-interval" <> metavar "BLOCKS" <> help "Interval between snapshots in blocks" <> showDefault <> value 256)

preservedCyclesOptions :: Parser Int
preservedCyclesOptions = option auto (long "preserved-cycles" <> metavar "BLOCKS" <> help "Preserved cycles constant, may be different for alphanet" <> showDefault <> value 5)

payoutDelayOptions :: Parser Int
payoutDelayOptions = option auto (long "payout-delay" <> metavar "BLOCKS" <> help "Delay in cycles to pay out delegators later or earlier than rewards unlocking" <> showDefault <> value 0)

accountOptions :: Parser T.Text
accountOptions = T.pack <$> strOption (long "account" <> metavar "ACCOUNT" <> help "Account name or KT1 address")

monitorOptions ∷ Parser Command
monitorOptions = pure Monitor

payoutOptions ∷ Parser Command
payoutOptions = Payout <$> noDryRunOptions <*> continuousOptions <*> noPasswordOptions

noDryRunOptions :: Parser Bool
noDryRunOptions = switch (long "no-dry-run" <> help "Really transfer Tezzies")

continuousOptions :: Parser Bool
continuousOptions = switch (long "continuous" <> help "Run continuously")

noPasswordOptions :: Parser Bool
noPasswordOptions = switch (long "no-password" <> help "Do not prompt for password, assume unencrypted account")
