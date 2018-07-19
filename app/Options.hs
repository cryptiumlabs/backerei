module Options where

import           Foundation
import           Options.Applicative

data Options = Options {
  optionsCommand :: Command
}

data Command =
  Version

options ∷ Parser Options
options = Options <$> commandOptions

commandOptions ∷ Parser Command
commandOptions = subparser (
  command "version" (info versionOptions (progDesc "Display program version information"))
  )

versionOptions ∷ Parser Command
versionOptions = pure Version
