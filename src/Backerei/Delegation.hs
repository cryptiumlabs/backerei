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
  selfBalance <- RPC.balanceAt config snapshotBlockHash delegate
  stakingBalance <- RPC.stakingBalance config snapshotBlockHash delegate
  if selfBalance P.+ P.sum balances /= stakingBalance then error "should not happen" else return ()
  return (((delegate, selfBalance) : zip delegators balances), stakingBalance)

totalRewards :: RPC.Config -> Int -> T.Text -> IO Tezzies
totalRewards config cycle delegate = do
  bakingRights <- filter ((==) 0 . bakingPriority) `fmap` RPC.bakingRightsFor config "head" delegate cycle
  endorsingRights <- RPC.endorsingRightsFor config "head" delegate cycle
  let bakingReward :: Tezzies
      bakingReward = 16
      endorsingReward :: Tezzies
      endorsingReward = 2
      totalReward :: Tezzies
      totalReward = (bakingReward P.* fromIntegral (P.length bakingRights)) P.+ (endorsingReward P.* fromIntegral (P.length endorsingRights))
  return totalReward

calculateRewardsFor :: RPC.Config -> Int -> T.Text -> Tezzies -> Rational -> IO ([(T.Text, Tezzies)], Tezzies)
calculateRewardsFor config cycle delegate rewards fee = do
  (balances, stakingBalance) <- getContributingBalancesFor config cycle delegate
  let totalBalance :: Tezzies
      totalBalance = P.sum $ fmap snd balances
  return (fmap (\(x, y) -> (x, y P.* (1 P.- P.fromRational fee) P.* rewards P./ totalBalance)) balances, stakingBalance)
