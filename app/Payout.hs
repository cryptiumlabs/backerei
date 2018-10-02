module Payout where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as M
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Foundation
import qualified Prelude               as P
import           System.Exit
import qualified System.Posix.Pty      as P
import qualified System.Process        as P

import qualified Backerei.Delegation   as Delegation
import qualified Backerei.RPC          as RPC
import qualified Backerei.Types        as RPC

import           Config
import           DB

payout :: Config -> Bool -> Maybe T.Text -> IO ()
payout (Config baker host port from fee databasePath accountDatabasePath clientPath clientConfigFile startingCycle cycleLength snapshotInterval _) noDryRun fromPassword = do
  let conf = RPC.Config host port

      maybeUpdateEstimatesForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
        if M.member cycle payouts then return (db, False) else do
          T.putStrLn $ T.concat ["Updating DB with estimates for cycle ", T.pack $ P.show cycle, "..."]
          estimatedRewards <- Delegation.estimatedRewards conf cycleLength cycle baker
          ((bakerBondReward, bakerFeeReward, bakerLooseReward, bakerTotalReward), calculated, stakingBalance) <- Delegation.calculateRewardsFor conf cycleLength snapshotInterval cycle baker estimatedRewards fee
          let bakerRewards = BakerRewards bakerBondReward bakerFeeReward bakerLooseReward bakerTotalReward
              delegators = M.fromList $ fmap (\(addr, balance, payout) -> (addr, DelegatorPayout balance payout Nothing Nothing)) calculated
              cyclePayout = CyclePayout stakingBalance fee estimatedRewards bakerRewards [] Nothing Nothing delegators
          return (db { dbPayoutsByCycle = M.insert cycle cyclePayout payouts }, True)
      maybeUpdateEstimates db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle = RPC.levelCycle currentLevel
            knownCycle   = currentCycle + 5
        foldFirst db (fmap maybeUpdateEstimatesForCycle [startingCycle .. knownCycle])

      maybeUpdateActualForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
        case M.lookup cycle payouts of
          Nothing -> error "should not happen: missed lookup"
          Just cyclePayout -> do
            if isJust (cycleFinalTotalRewards cyclePayout) then do
              return (db, False)
            else do
              T.putStrLn $ T.concat ["Updating DB with actual earnings for cycle ", T.pack $ P.show cycle, "..."]
              stolen <- Delegation.stolenBlocks conf cycleLength cycle baker
              let stolenBlocks = fmap (\(a, b, c, d, e) -> StolenBlock a b c d e) stolen
              hash <- Delegation.hashToQuery conf (cycle + 1) cycleLength
              frozenBalanceByCycle <- RPC.frozenBalanceByCycle conf hash baker
              let [thisCycle] = P.filter ((==) cycle . RPC.frozenCycle) frozenBalanceByCycle
                  feeRewards = RPC.frozenFees thisCycle
                  extraRewards = feeRewards
                  realizedRewards = feeRewards P.+ RPC.frozenRewards thisCycle
                  estimatedRewards = cycleEstimatedTotalRewards cyclePayout
                  paidRewards = estimatedRewards P.+ extraRewards
                  realizedDifference = realizedRewards P.- paidRewards
                  estimatedDifference = estimatedRewards P.- paidRewards
                  finalTotalRewards = CycleRewards realizedRewards paidRewards realizedDifference estimatedDifference
              if estimatedDifference > 0 then fail "should not happen: positive difference" else return ()
              ((bakerBondReward, bakerFeeReward, bakerLooseReward, bakerTotalReward), calculated, _) <- Delegation.calculateRewardsFor conf cycleLength snapshotInterval cycle baker paidRewards fee
              let bakerRewards = BakerRewards bakerBondReward bakerFeeReward bakerLooseReward bakerTotalReward
                  estimatedDelegators = cycleDelegators cyclePayout
                  delegators = M.fromList $ fmap (\(addr, balance, payout) -> (addr, DelegatorPayout balance (delegatorEstimatedRewards $ estimatedDelegators M.! addr) (Just payout) Nothing)) calculated
              return (db { dbPayoutsByCycle = M.insert cycle (cyclePayout { cycleStolenBlocks = stolenBlocks, cycleFinalTotalRewards = Just finalTotalRewards,
                cycleFinalBakerRewards = Just bakerRewards, cycleDelegators = delegators }) payouts }, True)
      maybeUpdateActual db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle = RPC.levelCycle currentLevel
            knownCycle   = currentCycle - 1
        foldFirst db (fmap maybeUpdateActualForCycle [startingCycle .. knownCycle])

      maybePayoutDelegatorForCycle cycle (address, delegator) db = do
        case (delegatorPayoutOperationHash delegator, delegatorFinalRewards delegator) of
          (Just _, _)  -> return (db, False)
          (Nothing, Just amount) | amount == 0 -> return (db, False)
          (Nothing, Just amount) | otherwise -> do
            T.putStrLn $ T.concat ["For cycle ", T.pack $ P.show cycle, " delegator ", address, " should be paid ", T.pack $ P.show amount, " XTZ"]
            updatedDelegator <-
              if noDryRun then do
                let args = ["-c", clientConfigFile, "transfer", T.pack $ P.show amount, "from", from, "to", address, "--fee", "0.0"]
                T.putStrLn $ T.concat ["Running '", T.intercalate " " (clientPath : args), "' in a pty"]
                (pty, handle) <- P.spawnWithPty Nothing True (T.unpack clientPath) (fmap T.unpack args) (80, 80)
                waitASecond
                P.threadWaitReadPty pty
                stderr <- P.readPty pty
                P.threadWaitWritePty pty
                P.writePty pty (B.pack $ T.unpack $ case fromPassword of Just pass -> T.concat [pass, "\n"]; Nothing -> "")
                waitASecond
                code <- P.waitForProcess handle
                stdout <- P.readPty pty
                P.closePty pty
                if code /= ExitSuccess then do
                  T.putStrLn $ T.concat ["Failure: ", T.pack $ P.show (code, stdout, stderr)]
                  exitFailure
                else do
                  let lines     = T.lines $ T.pack $ B.unpack stdout
                      start     = "Operation hash: "
                      filtered  = filter (\l -> T.take (T.length start) l == start) lines
                  case filtered of
                    [line] -> do
                      let hash = T.drop (T.length start) $ T.filter ((/=) '\r') line
                      T.putStrLn $ T.concat ["Operation hash: ", hash]
                      return delegator { delegatorPayoutOperationHash = Just hash }
                    _ -> do
                      T.putStrLn $ T.concat ["Expected operation hash but not found!"]
                      exitFailure
              else return delegator
            return (db { dbPayoutsByCycle = M.adjust (\c -> c { cycleDelegators = M.insert address updatedDelegator $ cycleDelegators c }) cycle $ dbPayoutsByCycle db }, noDryRun)
      maybePayoutForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
        case M.lookup cycle payouts of
          Nothing -> error "should not happen: missed lookup"
          Just cyclePayout -> do
            let delegators = cycleDelegators cyclePayout
            foldFirst db (fmap (maybePayoutDelegatorForCycle cycle) (M.toList delegators))
      maybePayout db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle  = RPC.levelCycle currentLevel
            unlockedCycle = currentCycle - 6
        foldFirst db (fmap maybePayoutForCycle [startingCycle .. unlockedCycle])

     {-
      maybeCalculateBalancesByCycle mainDB cycle db = do
        let history = accountHistory db
        case M.lookup cycle history of
          Just _  -> return (db, False)
          Nothing -> do
            T.putStrLn $ T.concat ["Calculating balances for cycle ", T.pack $ P.show cycle, "..."]
            let unlockedCycle = cycle - 5
            T.putStrLn $ T.concat ["Calculating snapshot level for cycle ", T.pack $ P.show unlockedCycle, "..."]
            snapshot <- if unlockedCycle < startingCycle then return 0 else Delegation.snapshotLevel conf unlockedCycle cycleLength snapshotInterval
            let cycleStart = cycle P.* cycleLength -- TODO Was the first cycle 0? This seems to calculate the wrong block.
                              -- oh actually need to subtract pending rewards
                previousState     = M.lookup (cycle - 1) history
                snapshotCycle     = unlockedCycle - 1
                snapshotState     = M.lookup snapshotCycle history
                previousHeight    = case previousState of Just s -> stateStartHeight s; Nothing -> 0
                previousRemainder = case previousState of Just s -> accountStartingBalance (stateRemainder s); Nothing -> 0
                txs = P.filter (\tx -> let b = txBlock tx in b >= previousHeight && b < cycleStart) (accountTxs db)
                difference acct   = P.sum $ fmap (\tx -> (case txKind tx of Debit -> -1; Credit -> 1) P.* txAmount tx) $ P.filter ((==) acct . txAccount) txs
                previousBalance acct = case previousState of Just s -> (case M.lookup acct (statePreferred s) of Just x -> accountFinalBalance x; Nothing -> 0); Nothing -> 0
                snapshotBalance acct = case snapshotState of Just s -> (case M.lookup acct (statePreferred s) of Just x -> accountFinalBalance x; Nothing -> 0); Nothing -> 0
                remainderDiff = difference ""
                initialRemainderBalance = previousRemainder P.+ remainderDiff
            T.putStrLn $ T.concat ["Fetching snapshot hash for level ", T.pack $ P.show snapshot, "..."]
            snapshotHash <- Delegation.blockHashByLevel conf snapshot
            totalBalance <- if unlockedCycle < startingCycle then return 0 else RPC.delegateBalanceAt conf snapshotHash baker
            T.putStrLn $ T.concat ["Total balance: ", T.pack $ P.show totalBalance]
            T.putStrLn $ T.concat ["Difference for remainder account: ", T.pack $ P.show remainderDiff, ", initial balance for remainder account: ", T.pack $ P.show initialRemainderBalance]
            let (estimatedBakerRewards, estimatedOtherRewards, finalBakerRewards) =
                  case M.lookup unlockedCycle (dbPayoutsByCycle mainDB) of
                    Just cycleRewards ->
                      let estimatedBakerRewards = cycleEstimatedBakerRewards cycleRewards
                          estimatedOtherRewards = bakerTotalRewards estimatedBakerRewards P.- bakerBondRewards estimatedBakerRewards
                          Just finalBakerRewards = cycleFinalBakerRewards cycleRewards
                      in (estimatedBakerRewards, estimatedOtherRewards, finalBakerRewards)
                    Nothing -> (BakerRewards 0 0 0 0, 0, BakerRewards 0 0 0 0)
            (totalPreferred, totalNet, afterRewards) <- flip foldM (0, 0, []) (\(rewards, balance, states) (name, ratio) -> do
              let previous = previousBalance name
                  diff = difference name
                  net = previous P.+ diff
                  pastNet = snapshotBalance name
                  fraction = if totalBalance == 0 then 0 else pastNet P./ totalBalance
                  bondRewards = bakerBondRewards estimatedBakerRewards P.* fraction
                  otherRewards = estimatedOtherRewards P.* fraction P.* P.fromRational ratio
                  totalRewards = bondRewards P.+ otherRewards
              T.putStrLn $ T.concat ["Initial balance for account ", name, ": ", T.pack $ P.show previous, ", difference: ", T.pack $ P.show diff, ", net now: ", T.pack $ P.show net,
                ", net at snapshot: ", T.pack $ P.show pastNet, ", fraction: ", T.pack $ P.show fraction, ", split: ", T.pack $ P.show ratio, ", bond rewards: ",
                T.pack $ P.show bondRewards, ", other rewards: ", T.pack $ P.show otherRewards, ", total rewards: ", T.pack $ P.show totalRewards]
              let cycleState = AccountCycleState net totalRewards (net P.+ totalRewards)
              return (rewards P.+ totalRewards, balance P.+ (net P.+ totalRewards), (name, cycleState) : states)) (accountsPreferred db)
            T.putStrLn $ T.concat ["Total rewards: ", T.pack $ P.show $ bakerTotalRewards estimatedBakerRewards, ", other rewards: ", T.pack $ P.show estimatedOtherRewards,
              ", total preferred: ", T.pack $ P.show totalPreferred, ", total net: ", T.pack $ P.show totalNet]
            let remainderRewards = bakerTotalRewards finalBakerRewards P.- totalPreferred
            cycleStartHash <- Delegation.blockHashByLevel conf cycleStart
            remainderBalance <- RPC.delegateBalanceAt conf cycleStartHash baker
            let remainderNet = remainderBalance P.- totalNet
            T.putStrLn $ T.concat ["Remainder realized rewards: ", T.pack $ P.show remainderRewards, ", remainder net: ", T.pack $ P.show remainderNet]
            let state = AccountsState cycleStart (M.fromList afterRewards) (AccountCycleState initialRemainderBalance remainderRewards remainderNet)
            return (db { accountHistory = M.insert cycle state $ accountHistory db }, True)
      -}

      maybePayoutAccountsForCycle mainDB cycle db = do
        -- Pay out released rewards (looking up balances) as virtual transactions, then mark cycle paid.
        let unlockedCycle = cycle P.- 6
        if unlockedCycle < startingCycle then return (db, False) else do
          let history = accountHistory db
              state = history M.! unlockedCycle
          if statePaid state then return (db, False) else do
            let cycleStart = cycle P.* cycleLength
                makeTx :: (T.Text, AccountCycleState) -> VirtualTx
                makeTx (account, AccountCycleState _ _ (Just finalRewards)) = VirtualTx "" account cycleStart finalRewards
                txs = fmap makeTx (M.toList $ statePreferred state)
                newState = state { statePaid = True }
            return (db { accountVtxs = accountVtxs db P.++ txs, accountHistory = M.insert unlockedCycle newState (accountHistory db) }, True)

      maybeFetchAccountEstimatesForCycle mainDB cycle db = do
        -- Fetch estimates for future cycle and calculate exact payouts for just-completed cycle.
        let knownCycle = cycle + 5
        -- Figure out snapshot block for known cycle.
        -- Calculate account balances at snapshot block.
        -- Split estimated rewards accordingly.
        undefined

      maybePayoutAccountsAndFetchEstimatesForCycle mainDB cycle db = do
        foldFirst db [maybePayoutAccountsForCycle mainDB cycle, maybeFetchAccountEstimatesForCycle mainDB cycle]
      maybePayoutAccountsAndFetchEstimates mainDB db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle    = RPC.levelCycle currentLevel
            completedCycle  = currentCycle - 1
        foldFirst db (fmap (maybePayoutAccountsAndFetchEstimatesForCycle mainDB) [startingCycle .. completedCycle])

      maybeFetchOperationsByLevel level db = do
        T.putStrLn $ T.concat ["Scanning operations in level ", T.pack $ P.show level, "..."]
        hash <- Delegation.blockHashByLevel conf level
        operations <- RPC.operations conf hash
        let assoc     = P.concatMap (\o -> fmap ((,) (RPC.operationHash o)) (RPC.operationContents o)) operations
            matching  = P.filter (\(_, contents) -> RPC.opcontentsKind contents == "transaction" && (RPC.opcontentsSource contents == Just baker || RPC.opcontentsDestination contents == Just baker)) assoc
        txs <- flip mapM matching $ \(hash, contents) -> do
          T.putStrLn $ T.concat ["Enter account name for operation: ", T.pack $ P.show (hash, contents), ":"]
          account <- T.getLine
          let kind = if RPC.opcontentsSource contents == Just baker then Debit else Credit
          return $ AccountTx hash account kind level (let Just a = RPC.opcontentsAmount contents in a)
        return (db { accountLastBlockScanned = level, accountTxs = accountTxs db P.++ txs }, True)
      maybeFetchOperations db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let level = RPC.levelLevel currentLevel
        foldFirst db (fmap maybeFetchOperationsByLevel [(accountLastBlockScanned db + 1) .. level])

      step databasePath db = do
        case db of
          Nothing -> do
            T.putStrLn $ T.concat ["Creating new DB in file ", databasePath, "..."]
            return (DB M.empty, True)
          Just prev -> do
            foldFirst prev [maybeUpdateEstimates, maybeUpdateActual, maybePayout]
      loop = do
        updated <- withDB (T.unpack databasePath) (step databasePath)
        unless (not updated) loop

      stepAccounts accountDatabasePath db = do
        mainDB <- mustReadDB (T.unpack databasePath)
        case db of
          Nothing -> do
            T.putStrLn $ T.concat ["Creating new account DB in file ", accountDatabasePath, "..."]
            return (AccountDB (-1) [] [] [] M.empty, True)
          Just prev -> do
            -- Calculate rewards & updated balances.
            foldFirst prev [maybeFetchOperations, maybePayoutAccountsAndFetchEstimates mainDB]
      loopAccounts path = do
        updated <- withAccountDB (T.unpack path) (stepAccounts path)
        unless (not updated) (loopAccounts path)

  loop

  case accountDatabasePath of
    Just path -> loopAccounts path
    Nothing   -> return ()

waitASecond :: IO ()
waitASecond = threadDelay (P.round (1e6 :: Double))

foldFirst :: a -> [a -> IO (a, Bool)] -> IO (a, Bool)
foldFirst obj [] = return (obj, False)
foldFirst obj (act:rest) = do
  (new, updated) <- act obj
  if updated then return (new, updated) else foldFirst obj rest
