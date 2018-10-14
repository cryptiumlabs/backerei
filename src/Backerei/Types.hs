module Backerei.Types where

import qualified Data.Aeson       as A
import qualified Data.Aeson.Types as A
import           Data.Char
import           Data.Fixed
import           Data.List        (concatMap, zip)
import qualified Data.Text        as T
import qualified Data.Time.Clock  as C
import           Foundation
import           GHC.Generics
import qualified Prelude          as P

data XTZ

instance HasResolution XTZ where
  resolution _ = 1000000

newtype Tezzies = Tezzies { unTezzies :: Fixed XTZ }
  deriving (P.Read, P.Num, P.Real, P.RealFrac, P.Fractional, Eq, Ord, Generic)

instance P.Show Tezzies where
  show = P.show . unTezzies

instance A.FromJSON Tezzies where
  parseJSON val = do
    result <- A.parseJSON val
    let tez = P.read result :: Integer
    return (Tezzies (fromIntegral tez P./ 1000000))

instance A.ToJSON Tezzies where
  toJSON (Tezzies (MkFixed t)) = A.toJSON $ T.pack $ P.show t

data BlockHeader = BlockHeader {
  headerHash  :: T.Text,
  headerLevel :: Int
} deriving (Generic, Show)

data BlockMetadata = BlockMetadata {
  metadataProtocol       :: T.Text,
  metadataBaker          :: T.Text,
  metadataBalanceUpdates :: [BalanceUpdate]
} deriving (Generic, Show)

data BalanceUpdate = BalanceUpdate {
  updateKind     :: T.Text,
  updateCategory :: Maybe T.Text,
  updateContract :: Maybe T.Text,
  updateDelegate :: Maybe T.Text,
  updateLevel    :: Maybe Int,
  updateChange   :: Tezzies
} deriving (Generic, Show)

data EndorsingRight = EndorsingRight {
  endorsingSlots         :: [Int],
  endorsingDelegate      :: T.Text,
  endorsingEstimatedTime :: Maybe C.UTCTime,
  endorsingLevel         :: Int
} deriving (Generic, Show)

data BakingRight = BakingRight {
  bakingDelegate      :: T.Text,
  bakingPriority      :: Int,
  bakingEstimatedTime :: Maybe C.UTCTime,
  bakingLevel         :: Int
} deriving (Generic, Show)

data CurrentLevel = CurrentLevel {
  levelVotingPeriod         :: Int,
  levelExpectedCommitment   :: Bool,
  levelLevelPosition        :: Int,
  levelCyclePosition        :: Int,
  levelCycle                :: Int,
  levelLevel                :: Int,
  levelVotingPeriodPosition :: Int
} deriving (Generic, Show)

data CycleInfo = CycleInfo {
  cycleinfoRandomSeed   :: T.Text,
  cycleinfoRollSnapshot :: Int
} deriving (Generic, Show)

data Operation = Operation {
  operationHash     :: T.Text,
  operationProtocol :: T.Text,
  operationBranch   :: T.Text,
  operationContents :: [OperationContents]
} deriving (Generic, Show)

data OperationContents = OperationContents {
  opcontentsKind        :: T.Text,
  opcontentsFee         :: Maybe Tezzies,
  opcontentsLevel       :: Maybe Int,
  opcontentsMetadata    :: OperationMetadata,
  opcontentsSource      :: Maybe T.Text,
  opcontentsDestination :: Maybe T.Text,
  opcontentsAmount      :: Maybe Tezzies
} deriving (Generic, Show)

newtype OperationMetadata = OperationMetadata {
  opmetadataDelegate :: Maybe T.Text
} deriving (Generic, Show)

data FrozenBalanceByCycle = FrozenBalanceByCycle {
  frozenCycle   :: Int,
  frozenDeposit :: Tezzies,
  frozenFees    :: Tezzies,
  frozenRewards :: Tezzies
} deriving (Generic, Show)

instance A.FromJSON BlockHeader where
  parseJSON = customParseJSON

instance A.FromJSON BlockMetadata where
  parseJSON = customParseJSON

instance A.FromJSON EndorsingRight where
  parseJSON = customParseJSON

instance A.FromJSON BakingRight where
  parseJSON = customParseJSON

instance A.FromJSON CurrentLevel where
  parseJSON = customParseJSON

instance A.FromJSON CycleInfo where
  parseJSON = customParseJSON

instance A.FromJSON Operation where
  parseJSON = customParseJSON

instance A.FromJSON OperationContents where
  parseJSON = customParseJSON

instance A.FromJSON OperationMetadata where
  parseJSON = customParseJSON

instance A.FromJSON FrozenBalanceByCycle where
  parseJSON = customParseJSON

instance A.FromJSON BalanceUpdate where
  parseJSON = customParseJSON

jsonOptions ∷ A.Options
jsonOptions = A.defaultOptions {
  A.fieldLabelModifier = concatMap (\(i, c) -> if i > (0 :: Int) && isUpper c then ['_', toLower c] else [toLower c]) . zip [0..] . dropWhile isLower,
  A.constructorTagModifier = \(x:xs) -> toLower x : xs,
  A.omitNothingFields  = True,
  A.sumEncoding        = A.ObjectWithSingleField
}

customParseJSON :: (Generic a, A.GFromJSON A.Zero (Rep a)) => A.Value -> A.Parser a
customParseJSON = A.genericParseJSON jsonOptions

customToJSON :: (Generic a, A.GToJSON A.Zero (Rep a)) => a -> A.Value
customToJSON = A.genericToJSON jsonOptions

customToEncoding :: (Generic a, A.GToEncoding A.Zero (Rep a)) => a -> A.Encoding
customToEncoding = A.genericToEncoding jsonOptions
