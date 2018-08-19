module Payout where

import           Control.Monad
import qualified Data.Aeson          as A
import qualified Data.Map            as M
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Foundation
import           Options.Applicative
import qualified Prelude             as P

import qualified Backerei.Delegation as Delegation
import qualified Backerei.RPC        as RPC
import qualified Backerei.Types      as RPC

import           Config
import           DB

payout :: Config -> Bool -> IO ()
payout (Config baker host port from fee databasePath clientPath _) noDryRun = do
  let conf = RPC.Config host port
      maybeUpdateEstimatesForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
        if M.member cycle payouts then return (db, False) else do
          T.putStrLn $ T.concat ["Updating DB with estimates for cycle ", T.pack $ P.show cycle, "..."]
          estimatedRewards <- Delegation.estimatedRewards conf cycle baker
          ((bakerBondReward, bakerFeeReward, bakerLooseReward, bakerTotalReward), calculated, stakingBalance) <- Delegation.calculateRewardsFor conf cycle baker estimatedRewards fee
          let bakerRewards = BakerRewards bakerBondReward bakerFeeReward bakerLooseReward bakerTotalReward
              delegators = M.fromList $ fmap (\(addr, balance, payout) -> (addr, DelegatorPayout balance payout Nothing)) calculated
              cyclePayout = CyclePayout stakingBalance fee estimatedRewards bakerRewards Nothing Nothing delegators
          return (db { dbPayoutsByCycle = M.insert cycle cyclePayout payouts }, True)
      maybeUpdateEstimates db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle = RPC.levelCycle currentLevel
            knownCycle   = currentCycle + 5
        foldFirst db (fmap maybeUpdateEstimatesForCycle [11 .. knownCycle])
      step db = do
        case db of
          Nothing -> return (DB M.empty, True)
          Just prev -> do
            foldFirst prev [maybeUpdateEstimates]
      loop = do
        updated <- withDB (T.unpack databasePath) step
        unless (not updated) loop
  loop

foldFirst :: a -> [(a -> IO (a, Bool))] -> IO (a, Bool)
foldFirst obj [] = return (obj, False)
foldFirst obj (act:rest) = do
  (new, updated) <- act obj
  if updated then return (new, updated) else foldFirst obj rest

  {-
  totalRewards <- Delegation.totalRewards conf cycle baker
  (calculated, stakingBalance) <- Delegation.calculateRewardsFor conf cycle baker totalRewards fee
  T.putStrLn $ T.concat ["Staking balance: ", T.pack $ P.show stakingBalance, " XTZ"]
  T.putStrLn $ T.concat ["Total rewards: ", T.pack $ P.show totalRewards, " XTZ; less fee: ", T.pack $ P.show $ (totalRewards P.* (1 P.- P.fromRational fee))]
  forM_ (drop 1 calculated) $ \(x, y) -> do
    T.putStrLn $ T.concat [x, " should be paid ", T.pack $ P.show y]
  -}
    {-
    let cmd = [path, "transfer", T.pack $ P.show y, "from", from, "to", x, "--fee", "0.0"]
    T.putStrLn $ T.concat ["Running '", T.intercalate " " cmd, "'"]
    if noDryRun then do
      let proc = P.proc (T.unpack path) $ drop 1 $ fmap T.unpack cmd
      (code, stdout, stderr) <- P.readCreateProcessWithExitCode proc ""
      if code /= ExitSuccess then do
        T.putStrLn $ T.concat ["Failure: ", T.pack $ P.show (code, stdout, stderr)]
      else do
        T.putStrLn $ T.pack stdout
    else return ()
    -}
