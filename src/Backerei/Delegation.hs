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
  if snapshotBlockLevel /= blockHeight then error "should not happen" else return ()
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
      totalReward = (bakingReward P.* fromIntegral (P.length bakingRights)) P.+ (endorsingReward P.* fromIntegral (P.length endorsingRights))
  return totalReward

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
      bakerLooseReward = rewards P.- totalDelegatorRewards P.- bakerSelfReward P.- bakerFeeReward
      bakerTotalReward = bakerSelfReward P.+ bakerFeeReward P.+ bakerLooseReward
      bakerRewards = (bakerSelfReward, bakerFeeReward, bakerLooseReward, bakerTotalReward)
  if (bakerTotalReward P.+ totalDelegatorRewards /= rewards) then error "should not happen" else return ()
  return (bakerRewards, delegatorRewards, stakingBalance)
