module Backerei.Delegation where

import           Control.Monad
import           Data.List      (zip)
import qualified Data.Text      as T
import           Foundation
import qualified Prelude        as P

import qualified Backerei.RPC   as RPC
import           Backerei.Types

getContributingBalancesFor :: RPC.Config -> Int -> T.Text -> IO ([(T.Text, Tezzies)], Tezzies)
getContributingBalancesFor config cycle delegate = do
  (BlockHeader hash level) <- RPC.header config "head"
  (CycleInfo _ snapshot) <- RPC.cycleInfo config hash cycle
  {- Cycles are 4096 blocks long, snapshots happen once every 256 blocks. -}
  let blockHeight = (cycle - 7) * 4096 + ((snapshot + 1) * 256)
  (BlockHeader snapshotBlockHash snapshotBlockLevel) <- RPC.header config (T.concat [hash, "~", T.pack $ P.show $ level - blockHeight])
  if snapshotBlockLevel /= blockHeight then error "should not happen: tezos rpc fault" else return ()
  delegators <- RPC.delegatedContracts config snapshotBlockHash delegate
  balances <- mapM (RPC.balanceAt config snapshotBlockHash) delegators
  fullBalance <- RPC.delegateBalanceAt config snapshotBlockHash delegate
  frozenByCycle <- RPC.frozenBalanceByCycle config snapshotBlockHash delegate
  let totalFrozenRewards = foldl' (P.+) 0 (fmap frozenRewards frozenByCycle)
      selfBalance = fullBalance P.- totalFrozenRewards
  stakingBalance <- RPC.stakingBalanceAt config snapshotBlockHash delegate
  if selfBalance P.+ P.sum balances /= stakingBalance then error "should not happen" else return ()
  return (((delegate, selfBalance) : zip delegators balances), stakingBalance)

estimatedRewards :: RPC.Config -> Int -> T.Text -> IO Tezzies
estimatedRewards config cycle delegate = do
  bakingRights <- filter ((==) 0 . bakingPriority) `fmap` RPC.bakingRightsFor config "head" delegate cycle
  endorsingRights <- RPC.endorsingRightsFor config "head" delegate cycle
  let bakingReward :: Tezzies
      bakingReward = 16
      endorsingReward :: Tezzies
      endorsingReward = 2
      totalReward :: Tezzies
      totalReward = (bakingReward P.* fromIntegral (P.length bakingRights)) P.+ (endorsingReward P.* fromIntegral (P.sum $ fmap (P.length . endorsingSlots) endorsingRights))
  return totalReward

blockHashByLevel :: RPC.Config -> Int -> IO T.Text
blockHashByLevel config level = do
  (BlockHeader hashHead levelHead) <- RPC.header config "head"
  (BlockHeader hash' level') <- RPC.header config (T.concat [hashHead, "~", T.pack $ P.show $ levelHead - level])
  if level /= level' then error "should not happen: tezos rpc fault" else return ()
  return hash'

stolenBlocks :: RPC.Config -> Int -> T.Text -> IO [(Int, T.Text, Int, Tezzies, Tezzies)]
stolenBlocks config cycle delegate = do
  bakingRights <- filter ((<) 0 . bakingPriority) `fmap` RPC.bakingRightsFor config "head" delegate cycle
  stolen <- mconcat `fmap` (flip mapM bakingRights $ \(BakingRight _ priority _ level) -> do
    hash <- blockHashByLevel config level
    (BlockMetadata _ baker balanceUpdates) <- RPC.metadata config hash
    if baker /= delegate then return [] else do
      operations <- RPC.operations config hash
      let [update] = filter (\u -> updateKind u == "freezer" && updateCategory u == Just "rewards" && updateDelegate u == Just delegate) balanceUpdates
          reward = updateChange update
          fees = P.sum $ fmap (P.sum . fmap (fromMaybe 0 . opcontentsFee) . operationContents) operations
      return [(level, hash, priority, reward, fees)])
  return stolen

calculateRewardsFor :: RPC.Config -> Int -> T.Text -> Tezzies -> Rational -> IO ((Tezzies, Tezzies, Tezzies, Tezzies), [(T.Text, Tezzies, Tezzies)], Tezzies)
calculateRewardsFor config cycle delegate rewards fee = do
  (balances, stakingBalance) <- getContributingBalancesFor config cycle delegate
  let totalBalance :: Tezzies
      totalBalance = P.sum $ fmap snd balances
      feeTz :: Tezzies
      feeTz = P.fromRational fee
      (_, bakerBalance) = P.head balances
      bakerSelfReward = bakerBalance P.* rewards P./ totalBalance
      bakerFeeReward = feeTz P.* rewards P.* (totalBalance P.- bakerBalance) P./ totalBalance
      delegatorRewards = fmap (\(x, y) -> (x, y, y P.* (1 P.- feeTz) P.* rewards P./ totalBalance)) $ drop 1 balances
      totalDelegatorRewards = P.sum (fmap (\(_, _, r) -> r) delegatorRewards)
      {- Leftover from fixed-precision floor rounding. -}
      bakerLooseReward = rewards P.- totalDelegatorRewards P.- bakerSelfReward P.- bakerFeeReward
      bakerTotalReward = bakerSelfReward P.+ bakerFeeReward P.+ bakerLooseReward
      bakerRewards = (bakerSelfReward, bakerFeeReward, bakerLooseReward, bakerTotalReward)
  if (bakerTotalReward P.+ totalDelegatorRewards /= rewards) then error "should not happen: rewards mismatch" else return ()
  return (bakerRewards, delegatorRewards, stakingBalance)
