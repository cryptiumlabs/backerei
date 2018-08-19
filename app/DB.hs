module DB where

import qualified Data.Aeson               as A
import qualified Data.Aeson.Encode.Pretty as A
import qualified Data.Aeson.Types         as A
import qualified Data.ByteString.Lazy     as BL
import           Data.Char                (isLower, toLower)
import qualified Data.Map                 as M
import qualified Data.Text                as T
import           Foundation
import           GHC.Generics
import qualified Prelude                  as P
import qualified System.Directory         as D

import           Backerei.Types           (Tezzies)

withDB :: forall a . P.FilePath -> (Maybe DB -> IO (DB, a)) -> IO a
withDB path func = do
  exists <- D.doesFileExist path
  (updated, other) <- do
    if exists then do
      prev <- BL.readFile path
      case A.decode prev of
        Just config -> func config
        Nothing     -> error "could not decode DB"
    else do
      func Nothing
  BL.writeFile path $ A.encodePretty' prettyConfig updated
  return other

prettyConfig :: A.Config
prettyConfig = A.Config (A.Spaces 4) A.compare A.Generic False

data DB = DB {
  dbPayoutsByCycle :: M.Map Int CyclePayout
} deriving (Generic, Show)

data CyclePayout = CyclePayout {
  cycleStakingBalance        :: Tezzies,
  cycleFee                   :: Rational,
  cycleEstimatedTotalRewards :: Tezzies,
  cycleEstimatedBakerRewards :: BakerRewards,
  cycleStolenBlocks          :: [StolenBlock],
  cycleFinalTotalRewards     :: Maybe CycleRewards,
  cycleFinalBakerRewards     :: Maybe BakerRewards,
  cycleDelegators            :: M.Map T.Text DelegatorPayout
} deriving (Generic, Show)

data StolenBlock = StolenBlock {
  blockLevel    :: Int,
  blockHash     :: T.Text,
  blockPriority :: Int,
  blockReward   :: Tezzies,
  blockFees     :: Tezzies
} deriving (Generic, Show)

data CycleRewards = CycleRewards {
  rewardsRealized            :: Tezzies,
  rewardsPaid                :: Tezzies,
  rewardsRealizedDifference  :: Tezzies,
  rewardsEstimatedDifference :: Tezzies
} deriving (Generic, Show)

data BakerRewards = BakerRewards {
  bakerBondRewards  :: Tezzies,
  bakerFeeRewards   :: Tezzies,
  bakerLooseRewards :: Tezzies,
  bakerTotalRewards :: Tezzies
} deriving (Generic, Show)

data DelegatorPayout = DelegatorPayout {
  delegatorBalance             :: Tezzies,
  delegatorEstimatedRewards    :: Tezzies,
  delegatorFinalRewards        :: Maybe Tezzies,
  delegatorPayoutOperationHash :: Maybe T.Text
} deriving (Generic, Show)

instance A.FromJSON DB where
  parseJSON = customParseJSON

instance A.ToJSON DB where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON CyclePayout where
  parseJSON = customParseJSON

instance A.ToJSON CyclePayout where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON BakerRewards where
  parseJSON = customParseJSON

instance A.ToJSON BakerRewards where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON CycleRewards where
  parseJSON = customParseJSON

instance A.ToJSON CycleRewards where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON DelegatorPayout where
  parseJSON = customParseJSON

instance A.ToJSON DelegatorPayout where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON StolenBlock where
  parseJSON = customParseJSON

instance A.ToJSON StolenBlock where
  toJSON = customToJSON
  toEncoding = customToEncoding

jsonOptions ∷ A.Options
jsonOptions = A.defaultOptions {
  A.fieldLabelModifier = (\(h:t) -> toLower h : t) . dropWhile isLower,
  A.omitNothingFields  = True,
  A.sumEncoding        = A.ObjectWithSingleField
}

customParseJSON :: (Generic a, A.GFromJSON A.Zero (Rep a)) => A.Value -> A.Parser a
customParseJSON = A.genericParseJSON jsonOptions

customToJSON = A.genericToJSON jsonOptions

customToEncoding = A.genericToEncoding jsonOptions
