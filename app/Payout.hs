module Payout where

import           Control.Monad
import qualified Data.Aeson          as A
import qualified Data.Text           as T
import qualified Data.Text.IO        as T
import           Foundation
import           Options.Applicative
import qualified Prelude             as P

import qualified Backerei.Delegation as Delegation
import qualified Backerei.RPC        as RPC
import qualified Backerei.Types      as B

import           Config
import           DB

payout :: Config -> Bool -> IO ()
payout config noDryRun = do
  let conf  = RPC.Config (configHost config) (configPort config)
      from  = configFromAddress config
      baker = configBakerAddress config
      path  = configClientPath config
      fee   = configFee config
      cycle = 11
  totalRewards <- Delegation.totalRewards conf cycle baker
  (calculated, stakingBalance) <- Delegation.calculateRewardsFor conf cycle baker totalRewards fee
  T.putStrLn $ T.concat ["Staking balance: ", T.pack $ P.show stakingBalance, " XTZ"]
  T.putStrLn $ T.concat ["Total rewards: ", T.pack $ P.show totalRewards, " XTZ; less fee: ", T.pack $ P.show $ (totalRewards P.* (1 P.- P.fromRational fee))]
  forM_ (drop 1 calculated) $ \(x, y) -> do
    T.putStrLn $ T.concat [x, " should be paid ", T.pack $ P.show y]
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
