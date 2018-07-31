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

data BlockHeader = BlockHeader {
  headerHash  :: T.Text,
  headerLevel :: Int
} deriving (Generic, Show)

data EndorsingRight = EndorsingRight {
  endorsingSlots         :: [Int],
  endorsingDelegate      :: T.Text,
  endorsingEstimatedTime :: C.UTCTime,
  endorsingLevel         :: Int
} deriving (Generic, Show)

data BakingRight = BakingRight {
  bakingDelegate      :: T.Text,
  bakingPriority      :: Int,
  bakingEstimatedTime :: C.UTCTime,
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

instance A.FromJSON BlockHeader where
  parseJSON = customParseJSON

instance A.FromJSON EndorsingRight where
  parseJSON = customParseJSON

instance A.FromJSON BakingRight where
  parseJSON = customParseJSON

instance A.FromJSON CurrentLevel where
  parseJSON = customParseJSON

instance A.FromJSON CycleInfo where
  parseJSON = customParseJSON

jsonOptions ∷ A.Options
jsonOptions = A.defaultOptions {
  A.fieldLabelModifier = concatMap (\(i, c) -> if i > 0 && isUpper c then ['_', toLower c] else [toLower c]) . (zip [0..]) . dropWhile isLower,
  A.omitNothingFields  = True,
  A.sumEncoding        = A.ObjectWithSingleField
}

customParseJSON :: (Generic a, A.GFromJSON A.Zero (Rep a)) => A.Value -> A.Parser a
customParseJSON = A.genericParseJSON jsonOptions

customToJSON = A.genericToJSON jsonOptions

customToEncoding = A.genericToEncoding jsonOptions
