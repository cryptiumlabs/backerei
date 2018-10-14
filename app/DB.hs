module DB where

import qualified Data.Aeson                               as A
import qualified Data.Aeson.Encode.Pretty                 as A
import qualified Data.Aeson.Types                         as A
import qualified Data.ByteString.Lazy                     as BL
import           Data.Char                                (isLower, toLower)
import qualified Data.Map                                 as M
import qualified Data.Text                                as T
import           Foundation
import           GHC.Generics
import qualified Prelude                                  as P
import qualified System.AtomicWrite.Writer.LazyByteString as AW
import qualified System.Directory                         as D

import           Backerei.Types                           (Tezzies)

withDB :: forall a . P.FilePath -> (Maybe DB -> IO (DB, a)) -> IO a
withDB = withFile

withAccountDB :: forall a . P.FilePath -> (Maybe AccountDB -> IO (AccountDB, a)) -> IO a
withAccountDB = withFile

mustReadDB :: P.FilePath -> IO DB
mustReadDB path = withFile path $ \case
  Nothing -> error "db expected but not found"
  Just db ->
    return (db, db)

withFile :: forall a b . (A.ToJSON b, A.FromJSON b) => P.FilePath -> (Maybe b -> IO (b, a)) -> IO a
withFile path func = do
  exists <- D.doesFileExist path
  (updated, other) <-
    if exists then do
      prev <- BL.readFile path
      case A.decode prev of
        Just db -> func db
        Nothing -> error "could not decode DB"
    else
      func Nothing
  AW.atomicWriteFile path $ A.encodePretty' prettyConfig updated
  return other

prettyConfig :: A.Config
prettyConfig = A.Config (A.Spaces 4) A.compare A.Generic False

newtype DB = DB {
  dbPayoutsByCycle :: M.Map Int CyclePayout
} deriving (Generic, Show)

data AccountDB = AccountDB {
  accountLastBlockScanned :: Int,
  accountTxs              :: [AccountTx],
  accountVtxs             :: [VirtualTx],
  accountsPreferred       :: [(T.Text, [(Int, Rational)])],
  accountHistory          :: M.Map Int AccountsState
} deriving (Generic, Show)

data AccountsState = AccountsState {
  stateSnapshotHeight :: Int,
  stateTotalBalance   :: Tezzies,
  statePreferred      :: M.Map T.Text AccountCycleState,
  stateRemainder      :: AccountCycleState,
  stateFinalized      :: Bool,
  statePaid           :: Bool
} deriving (Generic, Show)

data AccountTx = AccountTx {
  txOperation :: T.Text,
  txAccount   :: T.Text,
  txKind      :: TxKind,
  txBlock     :: Int,
  txAmount    :: Tezzies
} deriving (Generic, Show)

data VirtualTx = VirtualTx {
  vtxFrom   :: T.Text,
  vtxTo     :: T.Text,
  vtxBlock  :: Int,
  vtxAmount :: Tezzies
} deriving (Generic, Show)

data TxKind = Debit | Credit deriving (Generic, Show)

data AccountCycleState = AccountCycleState {
  accountStakingBalance   :: Tezzies,
  accountSplit            :: Rational,
  accountEstimatedRewards :: Tezzies,
  accountFinalRewards     :: Maybe Tezzies
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

instance A.FromJSON AccountDB where
  parseJSON = customParseJSON

instance A.ToJSON AccountDB where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON AccountsState where
  parseJSON = customParseJSON

instance A.ToJSON AccountsState where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON AccountTx where
  parseJSON = customParseJSON

instance A.ToJSON AccountTx where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON VirtualTx where
  parseJSON = customParseJSON

instance A.ToJSON VirtualTx where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON TxKind where
  parseJSON = customParseJSON

instance A.ToJSON TxKind where
  toJSON = customToJSON
  toEncoding = customToEncoding

instance A.FromJSON AccountCycleState where
  parseJSON = customParseJSON

instance A.ToJSON AccountCycleState where
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
