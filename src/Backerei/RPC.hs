module Backerei.RPC where

import           Control.Applicative
import           Control.Monad
import qualified Data.Aeson                as A
import qualified Data.Base58String.Bitcoin as B
import qualified Data.ByteString.Base16    as B
import qualified Data.ByteString.Char8     as B
import           Data.Default.Class
import qualified Data.Map                  as M
import qualified Data.Text                 as T
import qualified Data.Vector               as V
import           Foundation                hiding (head)
import qualified Network.HTTP.Req          as R
import qualified Prelude                   as P

import           Backerei.Types

data Config = Config {
  configHost :: T.Text,
  configPort :: Int
}

defaultConfig :: Config
defaultConfig = Config "127.0.0.1" 8732

head :: T.Text
head = "head"

currentLevel :: Config -> T.Text -> IO CurrentLevel
currentLevel config hash = get config ["chains", "main", "blocks", hash, "helpers", "current_level"] mempty

header :: Config -> T.Text -> IO BlockHeader
header config hash = get config ["chains", "main", "blocks", hash, "header"] mempty

metadata :: Config -> T.Text -> IO BlockMetadata
metadata config hash = get config ["chains", "main", "blocks", hash, "metadata"] mempty

operations :: Config -> T.Text -> IO [Operation]
operations config hash = P.concat `fmap` (get config ["chains", "main", "blocks", hash, "operations"] mempty :: IO [[Operation]])

cycleInfo :: Config -> T.Text -> Int -> IO CycleInfo
cycleInfo config hash cycle = get config ["chains", "main", "blocks", hash, "context", "raw", "json", "cycle", T.pack (P.show cycle)] mempty

delegatedContracts :: Config -> T.Text -> T.Text -> IO [T.Text]
delegatedContracts config hash delegate = get config ["chains", "main", "blocks", hash, "context", "delegates", delegate, "delegated_contracts"] mempty

delegatedBalanceAt :: Config -> T.Text -> T.Text -> IO Tezzies
delegatedBalanceAt config hash delegate = get config ["chains", "main", "blocks", hash, "context", "delegates", delegate, "delegated_balance"] mempty

delegateBalanceAt :: Config -> T.Text -> T.Text -> IO Tezzies
delegateBalanceAt config hash delegate = get config ["chains", "main", "blocks", hash, "context", "delegates", delegate, "balance"] mempty

frozenBalanceAt :: Config -> T.Text -> T.Text -> IO Tezzies
frozenBalanceAt config hash delegate = get config ["chains", "main", "blocks", hash, "context", "delegates", delegate, "frozen_balance"] mempty

frozenBalanceByCycle :: Config -> T.Text -> T.Text -> IO [FrozenBalanceByCycle]
frozenBalanceByCycle config hash delegate = get config ["chains", "main", "blocks", hash, "context", "delegates", delegate, "frozen_balance_by_cycle"] mempty

totalFrozenRewardsAt :: Config -> T.Text -> T.Text -> IO Tezzies
totalFrozenRewardsAt config hash delegate = do
  frozenByCycle <- frozenBalanceByCycle config hash delegate
  return $ P.sum $ fmap frozenRewards frozenByCycle

frozenFeesForCycle :: Config -> T.Text -> T.Text -> Int -> IO Tezzies
frozenFeesForCycle config hash delegate cycle = do
  frozenByCycle <- frozenBalanceByCycle config hash delegate
  return $ P.sum $ frozenFees <$> P.filter ((==) cycle . frozenCycle) frozenByCycle

stakingBalanceAt :: Config -> T.Text -> T.Text -> IO Tezzies
stakingBalanceAt config hash delegate = get config ["chains", "main", "blocks", hash, "context", "delegates", delegate, "staking_balance"] mempty

balanceAt :: Config -> T.Text -> T.Text -> IO Tezzies
balanceAt config hash contract = get config ["chains", "main", "blocks", hash, "context", "contracts", contract, "balance"] mempty

counter :: Config -> T.Text -> T.Text -> IO Integer
counter config hash contract = P.read `fmap` get config ["chains", "main", "blocks", hash, "context", "contracts", contract, "counter"] mempty

managerKey :: Config -> T.Text -> T.Text -> IO (Maybe T.Text)
managerKey config hash contract = get config ["chains", "main", "blocks", hash, "context", "contracts", contract, "manager_key"] mempty

bakingRightsFor :: Config -> T.Text -> T.Text -> Int -> IO [BakingRight]
bakingRightsFor config hash delegate cycle = get config ["chains", "main", "blocks", hash, "helpers", "baking_rights"]
  ("delegate" R.=: delegate <> "cycle" R.=: cycle)

endorsingRightsFor :: Config -> T.Text -> T.Text -> Int -> IO [EndorsingRight]
endorsingRightsFor config hash delegate cycle = get config ["chains", "main", "blocks", hash, "helpers", "endorsing_rights"]
  ("delegate" R.=: delegate <> "cycle" R.=: cycle)

block :: Config -> T.Text -> IO A.Value
block config hash = get config ["chains", "main", "blocks", hash] mempty

blocks :: Config -> IO [[T.Text]]
blocks config = get config ["chains", "main", "blocks"] mempty

protocols :: Config -> IO [T.Text]
protocols config = get config ["protocols"] mempty

sendTezzies :: Config -> T.Text -> T.Text -> [(T.Text, Tezzies)] -> (T.Text -> T.Text -> IO T.Text) -> IO T.Text
sendTezzies config from fromName dests sign = do
  currentCounter <- counter config head from
  let txns = fmap (\((dest, amount), counter) -> A.toJSON $ M.fromList [("kind" :: T.Text, A.String "transaction"), ("amount", A.toJSON amount), ("source", A.toJSON from),
                ("destination", A.String dest), ("storage_limit", A.String (if T.take 2 dest == "KT" then "4" else "0")), ("gas_limit", A.String (if T.take 2 dest == "KT" then "1000000" else "16000")), ("fee", A.toJSON $ Tezzies $ (if T.take 2 dest == "KT" then 0.1 else 0.002120)), ("counter", A.toJSON $ P.show counter)]) (P.zip dests [currentCounter + 1 ..])
  (BlockHeader hashHead _ _ _) <- header config head
  (BlockMetadata protocolHead _ _) <- metadata config hashHead
  let fakeSignature = "edsigtXomBKi5CTRf5cjATJWSyaRvhfYNHqSUGrn4SdbYRcGwQrUGjzEfQDTuqHhuA8b2d8NarZjz8TRf65WkpQmo423BtomS8Q" :: T.Text
      runJSON = A.toJSON $ M.fromList [("operation" :: T.Text, A.toJSON $ M.fromList [("branch" :: T.Text, A.String hashHead), ("contents", A.toJSON txns), ("signature", A.toJSON fakeSignature)]), ("chain_id", A.String "NetXdQprcVkpaWU")]
  (RunResult contents) <- post config ["chains", "main", "blocks", head, "helpers", "scripts", "run_operation"] mempty runJSON
  let succeeded = P.filter ((==) "applied" . opresultStatus . (\(Just x) -> x) . opmetadataOperationResult . opcontentsMetadata) contents
  when (P.length succeeded /= P.length dests) $ error $ show ("simulation failure", P.filter ((/=) "applied" . opresultStatus . (\(Just x) -> x) . opmetadataOperationResult . opcontentsMetadata) contents)
  let signJSON = A.toJSON $ M.fromList [("branch" :: T.Text, A.String hashHead), ("contents", A.toJSON txns)]
  (bytes :: T.Text) <- post config ["chains", "main", "blocks", head, "helpers", "forge", "operations"] mempty signJSON
  signature <- sign fromName bytes
  let base16Signature = T.pack $ B.unpack $ B.encode $ B.toBytes $ B.fromText signature
      stripped = T.drop 10 $ T.take (T.length base16Signature - 8) base16Signature
      signedOperation = bytes <> stripped
      preapplyJSON = A.toJSON $ V.singleton $ M.fromList [("branch" :: T.Text, A.String hashHead), ("contents", A.toJSON txns), ("signature", A.toJSON signature), ("protocol", A.toJSON protocolHead)]
  (_ :: A.Value) <- post config ["chains", "main", "blocks", head, "helpers", "preapply", "operations"] mempty preapplyJSON
  post config ["injection", "operation"] mempty signedOperation

get :: (A.FromJSON a) => Config -> [T.Text] -> R.Option 'R.Http -> IO a
get config path options = R.runReq def $ do
  r <- R.req R.GET
    (foldl' (R./:) (R.http (configHost config)) path)
    R.NoReqBody
    R.jsonResponse
    (R.port (configPort config) <> R.responseTimeout 600000000 <> options)
  return (R.responseBody r)

post :: (A.ToJSON a, A.FromJSON b) => Config -> [T.Text] -> R.Option 'R.Http -> a -> IO b
post config path options value = R.runReq def $ do
  r <- R.req R.POST
    (foldl' (R./:) (R.http (configHost config)) path)
    (R.ReqBodyJson value)
    R.jsonResponse
    (R.port (configPort config) <> R.responseTimeout 600000000 <> R.header "Content-Type" "application/json" <> options)
  return (R.responseBody r)
