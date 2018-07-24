module Backerei.Delegation where

import           Control.Monad
import           Data.List      (zip)
import qualified Data.Text      as T
import           Foundation
import qualified Prelude        as P

import qualified Backerei.RPC   as RPC
import           Backerei.Types

getContributingBalancesFor :: RPC.Config -> Int -> T.Text -> IO [(T.Text, T.Text)]
getContributingBalancesFor config cycle delegate = do
  (BlockHeader hash level) <- RPC.header config "head"
  (CycleInfo _ snapshot) <- RPC.cycleInfo config hash cycle
  {- Cycles are 4096 blocks long, snapshots happen once every 256 blocks. -}
  let blockHeight = (cycle - 7) * 4096 + ((snapshot + 1) * 256)
  (BlockHeader snapshotBlockHash snapshotBlockLevel) <- RPC.header config (T.concat [hash, "~", T.pack $ P.show $ level - blockHeight])
  if snapshotBlockLevel /= blockHeight then error "should not happen" else return ()
  delegators <- RPC.delegatedContracts config snapshotBlockHash delegate
  balances <- mapM (RPC.balanceAt config hash) delegators
  return (zip delegators balances)
