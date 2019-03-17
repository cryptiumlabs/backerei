module Config where

import qualified Data.Aeson   as A
import           Data.Char    (isLower, toLower)
import qualified Data.Text    as T
import qualified Data.Yaml    as Y
import           Foundation
import           GHC.Generics
import qualified Prelude      as P

data Config = Config {
  configBakerAddress        :: T.Text,
  configHost                :: T.Text,
  configPort                :: Int,
  configFromAddress         :: T.Text,
  configFromAccountName     :: T.Text,
  configFees                :: [(Int, Rational)],
  configDatabasePath        :: T.Text,
  configAccountDatabasePath :: Maybe T.Text,
  configClientPath          :: T.Text,
  configClientConfigFile    :: T.Text,
  configStartingCycle       :: Int,
  configCycleLength         :: Int,
  configSnapshotInterval    :: Int,
  configTelegram            :: Maybe TelegramConfig,
  configRiemann             :: Maybe RiemannConfig,
  configPostPayoutScript    :: Maybe T.Text
} deriving (Generic)

data TelegramConfig = TelegramConfig {
  telegramToken               :: T.Text,
  telegramNotificationChannel :: T.Text
} deriving (Generic)

data RiemannConfig = RiemannConfig {
  riemannHost :: T.Text,
  riemannPort :: Int
} deriving (Generic)

loadConfig ∷ P.FilePath → IO (Maybe Config)
loadConfig = Y.decodeFile

writeConfig ∷ P.FilePath → Config → IO ()
writeConfig = Y.encodeFile

instance Y.FromJSON Config where
  parseJSON = customParseJSON

instance Y.ToJSON Config where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance Y.FromJSON RiemannConfig where
  parseJSON = customParseJSON

instance Y.ToJSON RiemannConfig where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance Y.FromJSON TelegramConfig where
  parseJSON = customParseJSON

instance Y.ToJSON TelegramConfig where
  toJSON = customToJSON
  toEncoding = customToEncoding

jsonOptions ∷ A.Options
jsonOptions = A.defaultOptions {
  A.fieldLabelModifier = (\(h:t) -> toLower h : t) . dropWhile isLower,
  A.omitNothingFields  = True,
  A.sumEncoding        = A.ObjectWithSingleField
}

customParseJSON :: (Generic a, A.GFromJSON A.Zero (Rep a)) => Y.Value -> Y.Parser a
customParseJSON = A.genericParseJSON jsonOptions

customToJSON :: (Generic a, A.GToJSON A.Zero (Rep a)) => a -> A.Value
customToJSON = A.genericToJSON jsonOptions

customToEncoding :: (Generic a, A.GToEncoding A.Zero (Rep a)) => a -> A.Encoding
customToEncoding = A.genericToEncoding jsonOptions
