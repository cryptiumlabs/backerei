module Payout where

import           Control.Concurrent
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
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
          Just cyclePayout ->
            if isJust (cycleFinalTotalRewards cyclePayout) then
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
              when (estimatedDifference > 0) $ error "should not happen: positive difference"
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

      maybePayoutDelegatorForCycle cycle (address, delegator) db =
        case (delegatorPayoutOperationHash delegator, delegatorFinalRewards delegator) of
          (Nothing, Nothing) -> error "should not happen: neither operation hash nor final rewards"
          (Just _, _)  -> return (db, False)
          (Nothing, Just amount) | amount == 0 -> return (db, False)
          (Nothing, Just amount) -> do
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
                      let hash = T.drop (T.length start) $ T.filter ('\r' /=) line
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

      balanceAt :: AccountDB -> Int -> T.Text -> RPC.Tezzies
      balanceAt db height account =
        let txs   = P.filter (\tx -> txBlock tx <= height && txAccount tx == account) $ accountTxs db
            txb   = P.sum $ fmap (\tx -> (case txKind tx of Debit -> -1; Credit -> 1) P.* txAmount tx) txs
            vtxs  = P.filter (\vtx -> vtxBlock vtx <= height && (vtxFrom vtx == account || vtxTo vtx == account)) $ accountVtxs db
            vtxb  = P.sum $ fmap (\vtx -> (if vtxFrom vtx == account then -1 else 1) P.* vtxAmount vtx) vtxs
        in txb P.+ vtxb

      calculateRewards :: BakerRewards -> Maybe BakerRewards -> RPC.Tezzies -> RPC.Tezzies -> Rational -> RPC.Tezzies
      calculateRewards (BakerRewards estimatedBond _ _ estimatedTotal) _ balance totalBalance split =
        let fraction      = if balance == 0 then 0 else balance P./ totalBalance
            bondRewards   = estimatedBond
            bondNet       = fraction P.* bondRewards
            feeRewards    = 0
            otherRewards  = (estimatedTotal P.- estimatedBond) P.+ feeRewards
            otherNet      = otherRewards P.* fraction P.* P.fromRational split
            totalNet      = bondNet P.+ otherNet
        in totalNet

      maybePayoutAccountsForCycle cycle db = do
        -- Pay out released rewards (looking up balances) as virtual transactions, then mark cycle paid.
        let unlockedCycle = cycle P.- 6
        if unlockedCycle < startingCycle then return (db, False) else do
          let history = accountHistory db
              state = history M.! unlockedCycle
          if statePaid state then return (db, False) else do
            T.putStrLn $ T.concat ["Paying out internal accounts for cycle ", T.pack $ P.show unlockedCycle, "..."]
            let cycleStart = cycle P.* cycleLength
                makeTx :: (T.Text, AccountCycleState) -> VirtualTx
                makeTx (account, AccountCycleState _ _ _ (Just finalRewards)) = VirtualTx "" account cycleStart finalRewards
                makeTx _ = error "should not happen: no final rewards"
                txs = fmap makeTx (M.toList $ statePreferred state)
                newState = state { statePaid = True }
            return (db { accountVtxs = accountVtxs db P.++ txs, accountHistory = M.insert unlockedCycle newState (accountHistory db) }, True)

      maybeFetchAccountEstimatesForCycle mainDB cycle db = do
        -- Fetch estimates for future cycle and calculate exact payouts for just-completed cycle.
        let knownCycle = cycle + 5
            history = accountHistory db
        case M.lookup knownCycle history of
          Just _  -> return (db, False)
          Nothing -> do
            T.putStrLn $ T.concat ["Calculating estimated internal account rewards for cycle ", T.pack $ P.show knownCycle, "..."]
            -- Figure out snapshot block for known cycle.
            snapshot <- if knownCycle < startingCycle then return 0 else Delegation.snapshotLevel conf knownCycle cycleLength snapshotInterval
            snapshotHash <- Delegation.blockHashByLevel conf snapshot
            -- Calculate account balances at snapshot block.
            snapshotBalance <- if knownCycle < startingCycle then return 0 else RPC.delegateBalanceAt conf snapshotHash baker
            frozenRewards <- if knownCycle < startingCycle then return 0 else RPC.totalFrozenRewardsAt conf snapshotHash baker
            -- Split estimated rewards accordingly.
            let totalBalance = snapshotBalance P.- frozenRewards
                cyclePayout = dbPayoutsByCycle mainDB M.! knownCycle
                bakerRewards = cycleEstimatedBakerRewards cyclePayout
                accounts = fmap (\(account, splits) -> (account, let b = balanceAt db snapshot account in let split = snd $ P.last $ P.filter (\x -> fst x < snapshot) splits in AccountCycleState b split (calculateRewards bakerRewards Nothing b totalBalance split) Nothing)) (accountsPreferred db)
                remainderBalance = totalBalance P.- P.sum (fmap (accountStakingBalance . snd) accounts) -- TODO double-check with same calculation -- TODO subtract pending rewards
                remainderRewards = bakerTotalRewards bakerRewards P.- P.sum (fmap (accountEstimatedRewards . snd) accounts)
                preferred = M.fromList accounts
                remainder = AccountCycleState remainderBalance 0 remainderRewards Nothing
                state = AccountsState snapshot totalBalance preferred remainder False False
            T.putStrLn $ T.concat ["Estimated remainder balance: ", T.pack $ P.show remainderBalance, ", estimated remainder rewards: ", T.pack $ P.show remainderRewards]
            return (db { accountHistory = M.insert knownCycle state history }, True)

      maybeFetchAccountActualForCycle mainDB cycle db = do
        let finishedCycle = cycle - 1
            history = accountHistory db
            state = history M.! finishedCycle
        if finishedCycle < startingCycle || stateFinalized state then return (db, False) else do
          T.putStrLn $ T.concat ["Calculating final internal account rewards for cycle ", T.pack $ P.show finishedCycle, "..."]
          let cyclePayout = dbPayoutsByCycle mainDB M.! finishedCycle
              estimatedBakerRewards = cycleEstimatedBakerRewards cyclePayout
              Just finalBakerRewards = cycleFinalBakerRewards cyclePayout
              totalBalance = stateTotalBalance state
              updatedPreferred  = fmap (\(account, AccountCycleState balance split estimated Nothing) -> (account, AccountCycleState balance split estimated (Just $ calculateRewards estimatedBakerRewards (Just finalBakerRewards) balance totalBalance split))) $ M.toList $ statePreferred state
              remainderRewards  = bakerTotalRewards finalBakerRewards P.- P.sum (fmap (fromJust . accountFinalRewards . snd) updatedPreferred)
              remainder         = stateRemainder state
              updatedRemainder  = remainder { accountFinalRewards = Just remainderRewards }
              updatedState      = state { stateFinalized = True, statePreferred = M.fromList updatedPreferred, stateRemainder = updatedRemainder }
          T.putStrLn $ T.concat ["Estimated remainder rewards: ", T.pack $ P.show (accountEstimatedRewards remainder), ", final remainder rewards: ", T.pack $ P.show remainderRewards]
          return (db { accountHistory = M.insert finishedCycle updatedState history }, True)

      maybePayoutAccountsAndFetchEstimatesForCycle mainDB cycle db =
        foldFirst db [maybePayoutAccountsForCycle cycle, maybeFetchAccountEstimatesForCycle mainDB cycle, maybeFetchAccountActualForCycle mainDB cycle]
      maybePayoutAccountsAndFetchEstimates mainDB db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle = RPC.levelCycle currentLevel
        foldFirst db (fmap (maybePayoutAccountsAndFetchEstimatesForCycle mainDB) [startingCycle - 5 .. currentCycle])

      maybeFetchOperationsByLevel level db = do
        T.putStrLn $ T.concat ["Scanning operations in level ", T.pack $ P.show level, "..."]
        hash <- Delegation.blockHashByLevel conf level
        operations <- RPC.operations conf hash
        let assoc     = P.concatMap (\o -> fmap ((,) (RPC.operationHash o)) (RPC.operationContents o)) operations
            matching  = P.filter (\(_, contents) -> RPC.opcontentsKind contents == "transaction" && (RPC.opcontentsSource contents == Just baker || RPC.opcontentsDestination contents == Just baker)) assoc
        txs <- forM matching $ \(hash, contents) -> do
          T.putStrLn $ T.concat ["Enter account name for operation: ", T.pack $ P.show (hash, contents), ":"]
          account <- T.getLine
          let kind = if RPC.opcontentsSource contents == Just baker then Debit else Credit
          return $ AccountTx hash account kind level (let Just a = RPC.opcontentsAmount contents in a)
        return (db { accountLastBlockScanned = level, accountTxs = accountTxs db P.++ txs }, True)
      maybeFetchOperations db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let level = RPC.levelLevel currentLevel
        foldFirst db (fmap maybeFetchOperationsByLevel [(accountLastBlockScanned db + 1) .. level])

      step databasePath db =
        case db of
          Nothing -> do
            T.putStrLn $ T.concat ["Creating new DB in file ", databasePath, "..."]
            step databasePath (Just $ DB M.empty)
          Just prev -> do
            let loop = do
                  (res, updated) <- foldFirst prev [maybeUpdateEstimates, maybeUpdateActual, maybePayout]
                  if updated then loop else return (res, ())
            loop

      loop = withDB (T.unpack databasePath) (step databasePath)

      stepAccounts accountDatabasePath db = do
        mainDB <- mustReadDB (T.unpack databasePath)
        case db of
          Nothing -> do
            T.putStrLn $ T.concat ["Creating new account DB in file ", accountDatabasePath, "..."]
            stepAccounts accountDatabasePath (Just $ AccountDB (-1) [] [] [] M.empty)
          Just prev -> do
            let loop = do
                  (res, updated) <- foldFirst prev [maybeFetchOperations, maybePayoutAccountsAndFetchEstimates mainDB]
                  if updated then loop else return (res, ())
            loop

      loopAccounts path = withAccountDB (T.unpack path) (stepAccounts path)

  loop

  forM_ accountDatabasePath loopAccounts

waitASecond :: IO ()
waitASecond = threadDelay (P.round (1e6 :: Double))

foldFirst :: a -> [a -> IO (a, Bool)] -> IO (a, Bool)
foldFirst obj [] = return (obj, False)
foldFirst obj (act:rest) = do
  (new, updated) <- act obj
  if updated then return (new, updated) else foldFirst obj rest
