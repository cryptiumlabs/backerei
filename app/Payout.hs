module Payout where

import           Control.Concurrent
import           Control.Exception
import           Control.Monad
import qualified Data.ByteString.Char8 as B
import qualified Data.Map              as M
import           Data.Maybe            (fromJust)
import qualified Data.Text             as T
import qualified Data.Text.IO          as T
import           Foundation
import           Foundation.Collection ((!))
import qualified Prelude               as P
import           System.Exit
import qualified System.Posix.Pty      as P
import qualified System.Process        as P

import qualified Backerei.Delegation   as Delegation
import qualified Backerei.RPC          as RPC
import qualified Backerei.Types        as RPC

import           Config
import           DB

payout :: Config -> Bool -> Maybe T.Text -> Bool -> (T.Text -> IO ()) -> IO ()
payout (Config baker host port from fromName varyingFee databasePath accountDatabasePath clientPath clientConfigFile startingCycle cycleLength snapshotInterval preservedCycles payoutDelay babylonStartingCycle payEstimatedRewards _ _ maybePostPayoutScript) noDryRun fromPassword continuous notify = do
  let conf = RPC.Config host port

      isBabylon cycle = cycle >= babylonStartingCycle

      feeForCycle cycle = fromMaybe Config.defaultFee $
        snd . last <$> (nonEmpty $ filter ((>=) cycle . fst) varyingFee)

      maybeUpdateTimestampsForCycle cycle db = do
        let history = accountHistory db
        case M.lookup cycle history of
          Nothing -> return (db, False)
          Just state -> do
            case stateCycleStartTimestamp state of
              Nothing -> do
                let startLevel = Delegation.startingBlock cycle cycleLength
                hash <- Delegation.blockHashByLevel conf startLevel
                header <- RPC.header conf hash
                let startTimestamp = RPC.headerTimestamp header
                    newState = state { stateCycleStartTimestamp = Just startTimestamp }
                return (db { accountHistory = M.insert cycle newState history }, True)
              Just _  -> do
                case stateCycleEndTimestamp state of
                  Nothing -> do
                    let endLevel = Delegation.endingBlock cycle cycleLength
                    hash <- Delegation.blockHashByLevel conf endLevel
                    header <- RPC.header conf hash
                    let endTimestamp = RPC.headerTimestamp header
                        newState = state { stateCycleEndTimestamp = Just endTimestamp }
                    return (db { accountHistory = M.insert cycle newState history }, True)
                  Just _  -> return (db, False)

      maybeUpdateTimestamps db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle = RPC.levelCycle currentLevel
        foldFirst db (fmap maybeUpdateTimestampsForCycle [startingCycle .. currentCycle - 1])

      maybeUpdateEstimatesForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
            fee = feeForCycle cycle
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
            knownCycle   = currentCycle + preservedCycles
        (res, updated) <- foldFirst db (fmap maybeUpdateEstimatesForCycle [startingCycle .. knownCycle])
        return (res, (updated, return ()))

      maybeUpdateActualForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
            fee = feeForCycle cycle
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
              lostEndorsementRewards <- Delegation.lostEndorsementRewards conf cycleLength cycle baker
              T.putStrLn $ T.concat ["Lost endorsement rewards due to other-baker downtime for cycle ", T.pack $ P.show cycle, ": ", T.pack $ P.show lostEndorsementRewards]
              lostBakingRewards <- if isBabylon cycle then do
                lostRewards <- Delegation.lostBakingRewards conf cycleLength cycle baker
                T.putStrLn $ T.concat ["Lost baking rewards due to missing endorsements for cycle ", T.pack $ P.show cycle, ": ", T.pack $ P.show lostRewards]
                return lostRewards
                else return 0
              let thisCycle = filter ((==) cycle . RPC.frozenCycle) frozenBalanceByCycle ! 0
                  feeRewards = maybe 0 RPC.frozenFees thisCycle
                  extraRewards = feeRewards P.- lostEndorsementRewards P.- lostBakingRewards
                  realizedRewards = feeRewards P.+ maybe 0 RPC.frozenRewards thisCycle
                  estimatedRewards = cycleEstimatedTotalRewards cyclePayout
                  paidRewards = estimatedRewards P.+ extraRewards
                  realizedDifference = realizedRewards P.- paidRewards
                  estimatedDifference = estimatedRewards P.- paidRewards
                  finalTotalRewards = CycleRewards realizedRewards paidRewards realizedDifference estimatedDifference
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
        (res, updated) <- foldFirst db (fmap maybeUpdateActualForCycle [startingCycle .. knownCycle])
        return (res, (updated, return ()))

      maybePayoutDelegatorsForCycle cycle delegators db = do
        let needToPay' = if payEstimatedRewards then P.filter (\(_, delegator) -> case (delegatorPayoutOperationHash delegator, delegatorEstimatedRewards delegator) of (Nothing, amount) | amount > 0 -> True; _ -> False) $ M.toList delegators else P.filter (\(_, delegator) -> case (delegatorPayoutOperationHash delegator, delegatorFinalRewards delegator) of (Nothing, Just amount) | amount > 0 -> True; _ -> False) $ M.toList delegators
        needToPay <- (P.map (\(a, b, _) -> (a, b)) . P.filter (\(_, _, r) -> r)) `fmap` P.mapM (\(d, a) -> RPC.managerKey conf RPC.head d >>= \r -> pure (d, a, r /= Nothing)) needToPay'
        let toPay     = P.take 100 needToPay
        if null toPay then return (db, (False, return ())) else do
          forM_ toPay $ \(address, delegator) -> do
            if payEstimatedRewards then
              T.putStrLn $ T.concat ["For cycle ", T.pack $ P.show cycle, " delegator ", address, " should be paid estimated rewards of ", T.pack $ P.show $ delegatorEstimatedRewards delegator, " XTZ"]
            else
              T.putStrLn $ T.concat ["For cycle ", T.pack $ P.show cycle, " delegator ", address, " should be paid final rewards of ", T.pack $ P.show $ fromJust (delegatorFinalRewards delegator), " XTZ"]
          (updatedDelegators, action) <-
            if noDryRun then do
              let dests = if payEstimatedRewards then fmap (\(address, delegator) -> (address, delegatorEstimatedRewards delegator)) toPay else fmap (\(address, delegator) -> (address, let Just amount = delegatorFinalRewards delegator in amount)) toPay
              hash <- RPC.sendTezzies conf from fromName dests (sign clientPath clientConfigFile fromPassword)
              threadDelay 600000000
              if length toPay == length needToPay then do
                notify $ T.concat ["Payouts for cycle ", T.pack $ P.show cycle, " complete!"]
              else return ()
              let action = if length toPay == length needToPay then case maybePostPayoutScript of Nothing -> return (); Just script -> P.callCommand (T.unpack script) else return ()
              return (M.union (M.fromList $ fmap (\(address, delegator) -> (address, delegator { delegatorPayoutOperationHash = Just hash })) toPay) delegators, action)
            else return (delegators, return ())
          return (db { dbPayoutsByCycle = M.adjust (\c -> c { cycleDelegators = updatedDelegators }) cycle $ dbPayoutsByCycle db }, (noDryRun, action))
      maybePayoutForCycle cycle db = do
        let payouts = dbPayoutsByCycle db
        case M.lookup cycle payouts of
          Nothing -> error "should not happen: missed lookup"
          Just cyclePayout -> do
            let delegators = cycleDelegators cyclePayout
            let total = if payEstimatedRewards then P.sum $ fmap delegatorEstimatedRewards $ P.filter (isNothing . delegatorPayoutOperationHash) $ M.elems delegators else P.sum $ fmap (fromJust . delegatorFinalRewards) $ P.filter (isJust . delegatorFinalRewards) $ P.filter (isNothing . delegatorPayoutOperationHash) $ M.elems delegators
            if total == 0 then return (db, (False, return ())) else do
              balance <- RPC.balanceAt conf RPC.head from
              T.putStrLn $ T.concat ["Total payouts: ", T.pack $ P.show total, ", payout account balance: ", T.pack $ P.show balance]
              if balance < total then do
                T.putStrLn "Balance less than total required to pay cycle, aborting"
                return (db, (False, return ()))
              else maybePayoutDelegatorsForCycle cycle delegators db
      maybePayout db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle  = RPC.levelCycle currentLevel
            unlockedCycle = currentCycle - preservedCycles - payoutDelay - 1
        foldFirst3 db (fmap maybePayoutForCycle [startingCycle .. unlockedCycle])

      balanceAt :: AccountDB -> Int -> T.Text -> RPC.Tezzies
      balanceAt db height account =
        let txs   = P.filter (\tx -> txBlock tx <= height && txAccount tx == account) $ accountTxs db
            txb   = P.sum $ fmap (\tx -> (case txKind tx of Debit -> -1; Credit -> 1) P.* txAmount tx) txs
            vtxs  = P.filter (\vtx -> vtxBlock vtx < height && (vtxFrom vtx == account || vtxTo vtx == account)) $ accountVtxs db
            vtxb  = P.sum $ fmap (\vtx -> (if vtxFrom vtx == account then -1 else 1) P.* vtxAmount vtx) vtxs
        in txb P.+ vtxb

      calculateRewards :: BakerRewards -> Maybe (RPC.Tezzies, RPC.Tezzies, RPC.Tezzies, RPC.Tezzies) -> RPC.Tezzies -> RPC.Tezzies -> Rational -> AccountRewards
      calculateRewards (BakerRewards estimatedBond _ _ estimatedTotal) finalFees balance totalBalance split =
        let fraction      = if balance == 0 then 0 else balance P./ totalBalance
            bondRewards   = estimatedBond
            bondNet       = fraction P.* bondRewards
            feeNet        = case finalFees of Just (fees, lostEndorsementRewards, lostBakingRewards, total) -> balance P.* (fees P.- lostEndorsementRewards P.- lostBakingRewards) P./ total; Nothing -> 0
            otherRewards  = (estimatedTotal P.- estimatedBond)
            otherNet      = otherRewards P.* fraction P.* P.fromRational split
            selfNet       = bondNet P.+ feeNet
            totalNet      = selfNet P.+ otherNet
        in AccountRewards selfNet otherNet totalNet

      maybePayoutAccountsForCycle cycle db = do
        -- Pay out released rewards (looking up balances) as virtual transactions, then mark cycle paid.
        let unlockedCycle = cycle P.- preservedCycles - payoutDelay - 1
        if unlockedCycle < startingCycle then return (db, False) else do
          let history = accountHistory db
              state = history M.! unlockedCycle
          if statePaid state then return (db, False) else do
            T.putStrLn $ T.concat ["Paying out internal accounts for cycle ", T.pack $ P.show unlockedCycle, "..."]
            let cycleStart = cycle P.* cycleLength
                makeTx :: (T.Text, AccountCycleState) -> VirtualTx
                makeTx (account, AccountCycleState _ _ _ (Just finalRewards)) = VirtualTx "" account cycleStart (rewardsTotal finalRewards)
                makeTx _ = error "should not happen: no final rewards"
                txs = fmap makeTx (M.toList $ statePreferred state)
                newState = state { statePaid = True }
            let sweep = accountsSweep db
            T.putStrLn $ T.concat ["Sweeping all balances from accounts ", T.intercalate ", " sweep]
            let makeTx :: T.Text -> VirtualTx
                makeTx account = VirtualTx account "" cycleStart (balanceAt db cycleStart account)
                sweepTxs = filter (\vtx -> vtxAmount vtx > 0) $ fmap makeTx sweep
            return (db { accountVtxs = accountVtxs db P.++ txs P.++ sweepTxs, accountHistory = M.insert unlockedCycle newState (accountHistory db) }, True)

      maybeFetchAccountEstimatesForCycle mainDB cycle db = do
        -- Fetch estimates for future cycle and calculate exact payouts for just-completed cycle.
        let knownCycle = cycle + preservedCycles
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
                remainderBalance = totalBalance P.- P.sum (fmap (accountStakingBalance . snd) accounts)
                remainderRewards = bakerTotalRewards bakerRewards P.- P.sum (fmap (rewardsTotal . accountEstimatedRewards . snd) accounts)
                preferred = M.fromList accounts
                remainder = AccountCycleState remainderBalance 0 (AccountRewards 0 0 remainderRewards) Nothing
                state = AccountsState snapshot totalBalance preferred remainder False False Nothing Nothing
            T.putStrLn $ T.concat ["Estimated remainder balance: ", T.pack $ P.show remainderBalance, ", estimated remainder rewards: ", T.pack $ P.show remainderRewards]
            return (db { accountHistory = M.insert knownCycle state history }, True)

      maybeFetchAccountActualForCycle mainDB cycle db = do
        let finishedCycle = cycle - 1
            history = accountHistory db
            state = history M.! finishedCycle
        if finishedCycle < startingCycle || stateFinalized state then return (db, False) else do
          T.putStrLn $ T.concat ["Calculating final internal account rewards for cycle ", T.pack $ P.show finishedCycle, "..."]
          snapshot <- Delegation.snapshotLevel conf finishedCycle cycleLength snapshotInterval
          snapshotHash <- Delegation.blockHashByLevel conf snapshot
          snapshotBalance <- RPC.delegateBalanceAt conf snapshotHash baker
          hash <- Delegation.hashToQuery conf (finishedCycle + 2) cycleLength
          fees <- RPC.frozenFeesForCycle conf hash baker finishedCycle
          lostEndorsementRewards <- Delegation.lostEndorsementRewards conf cycleLength finishedCycle baker
          lostBakingRewards <- if isBabylon finishedCycle then Delegation.lostBakingRewards conf cycleLength finishedCycle baker else return 0
          T.putStrLn $ T.concat ["Total fees for cycle ", T.pack $ P.show finishedCycle, ": ", T.pack $ P.show fees]
          let cyclePayout = dbPayoutsByCycle mainDB M.! finishedCycle
              estimatedBakerRewards = cycleEstimatedBakerRewards cyclePayout
              Just finalBakerRewards = cycleFinalBakerRewards cyclePayout
              totalBalance = stateTotalBalance state
              updatedPreferred  = fmap (\(account, AccountCycleState balance split estimated Nothing) -> (account, AccountCycleState balance split estimated (Just $ calculateRewards estimatedBakerRewards (Just (fees, lostEndorsementRewards, lostBakingRewards, snapshotBalance)) balance totalBalance split))) $ M.toList $ statePreferred state
              remainderRewards  = bakerTotalRewards finalBakerRewards P.- P.sum (fmap (rewardsTotal . fromJust . accountFinalRewards . snd) updatedPreferred)
              remainder         = stateRemainder state
              updatedRemainder  = remainder { accountFinalRewards = Just $ AccountRewards 0 0 remainderRewards }
              updatedState      = state { stateFinalized = True, statePreferred = M.fromList updatedPreferred, stateRemainder = updatedRemainder }
          T.putStrLn $ T.concat ["Estimated remainder rewards: ", T.pack $ P.show (accountEstimatedRewards remainder), ", final remainder rewards: ", T.pack $ P.show remainderRewards]
          return (db { accountHistory = M.insert finishedCycle updatedState history }, True)

      maybePayoutAccountsAndFetchEstimatesForCycle mainDB cycle db =
        foldFirst db [maybePayoutAccountsForCycle cycle, maybeFetchAccountEstimatesForCycle mainDB cycle, maybeFetchAccountActualForCycle mainDB cycle]
      maybePayoutAccountsAndFetchEstimates mainDB db = do
        currentLevel <- RPC.currentLevel conf RPC.head
        let currentCycle = RPC.levelCycle currentLevel
        foldFirst db (fmap (maybePayoutAccountsAndFetchEstimatesForCycle mainDB) [startingCycle - preservedCycles .. currentCycle])

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
            foldFirst3 prev [maybeUpdateEstimates, maybeUpdateActual, maybePayout]

      loop = withDBLoop (T.unpack databasePath) (step databasePath)

      stepAccounts accountDatabasePath db = do
        mainDB <- mustReadDB (T.unpack databasePath)
        let loop db =
              case db of
                Nothing -> do
                  T.putStrLn $ T.concat ["Creating new account DB in file ", accountDatabasePath, "..."]
                  loop $ Just $ AccountDB (-1) [] [] [] [] M.empty
                Just prev -> do
                  (res, updated) <- foldFirst prev [maybeFetchOperations, maybePayoutAccountsAndFetchEstimates mainDB, maybeUpdateTimestamps]
                  if updated then loop (Just res) else return (res, ())
        loop db

      loopAccounts path = withAccountDB (T.unpack path) (stepAccounts path)

  let go = do
        loop
        forM_ accountDatabasePath loopAccounts
        when continuous $ do
          threadDelay 10000000
          go

  go

waitASecond :: IO ()
waitASecond = threadDelay (P.round (1e6 :: Double))

try' :: IO a ->  IO (Either IOException a)
try' =  try

foldFirst3 :: a -> [a -> IO (a, (Bool, IO ()))] -> IO (a, (Bool, IO ()))
foldFirst3 obj [] = return (obj, (False, return ()))
foldFirst3 obj (act:rest) = do
  (new, (updated, other)) <- act obj
  if updated then return (new, (updated, other)) else foldFirst3 obj rest

foldFirst :: a -> [a -> IO (a, Bool)] -> IO (a, Bool)
foldFirst obj [] = return (obj, False)
foldFirst obj (act:rest) = do
  (new, updated) <- act obj
  if updated then return (new, updated) else foldFirst obj rest

sign :: T.Text -> T.Text -> Maybe T.Text -> T.Text -> T.Text -> IO T.Text
sign clientPath clientConfigFile fromPassword account what = do
  let args = ["-c", clientConfigFile, "sign", "bytes", "0x03" <> what, "for", account]
  asText <- do
    case fromPassword of
      Just pass -> do
        T.putStrLn $ T.concat ["Running '", T.intercalate " " (clientPath : args), "' in a pty"]
        (pty, handle) <- P.spawnWithPty Nothing True (T.unpack clientPath) (fmap T.unpack args) (80, 80)
        waitASecond
        P.threadWaitReadPty pty
        stderr <- P.readPty pty
        P.threadWaitWritePty pty
        P.writePty pty (B.pack $ T.unpack $ T.concat [pass, "\n"])
        waitASecond
        code <- P.waitForProcess handle
        stdout <- P.readPty pty
        P.closePty pty
        if code /= ExitSuccess then do
          T.putStrLn $ T.concat ["Failure: ", T.pack $ P.show (code, stdout, stderr)]
          exitFailure
        else do
          let asText = T.pack $ B.unpack stdout
          return asText
      Nothing -> do
        T.putStrLn $ T.concat ["Running '", T.intercalate " " (clientPath : args), "' in a subprocess"]
        result <- try' $ P.createProcess (P.proc (P.show clientPath) (P.map P.show args)){ P.std_out = P.CreatePipe }
        case result of
          Left ex -> do
            T.putStrLn $ T.concat [ "Subprocess returned an exception: ", T.pack $ P.show ex ]
            exitFailure
          Right (_, Nothing , _, _) -> do
            T.putStrLn "Error: subprocess returned nothing"
            exitFailure
          Right (_, Just hout, _, _) -> do
            T.putStrLn "Success"
            asText <- T.hGetContents hout
            return asText
  T.putStrLn "> output of signature"
  T.putStrLn asText
  return $ T.drop 13 $ T.take (T.length asText - 2) asText
