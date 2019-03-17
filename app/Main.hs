{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Function                    (on, (&))
import qualified Data.Sequence                    as Seq
import qualified Data.Text                        as T
import qualified Data.Text.IO                     as T
import           Development.GitRev
import           Foundation
import qualified Network.Monitoring.Riemann.Event as Riemann
import qualified Network.Monitoring.Riemann.TCP   as Riemann
import           Options.Applicative
import qualified Prelude                          as P
import qualified Servant.Client                   as TG
import           System.Directory
import           System.Exit
import           System.IO
import qualified Telegram.Bot.API                 as TG
import           Text.PrettyPrint.ANSI.Leijen     hiding ((<$>), (<>))

import qualified Backerei.RPC                     as RPC
import qualified Backerei.Types                   as RPC

import           Config
import           Options
import           Payout

main ∷ IO ()
main = do
  ctx <- context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run =<< execParser opts

context ∷ IO Context
context = Context <$> getHomeDirectory

run ∷ Options → IO ()
run (Options configPath command) = do
  let withConfig ∷ (Config → IO ()) → IO ()
      withConfig func = do
        maybeConf <- loadConfig configPath
        case maybeConf of
          Nothing -> do
            T.putStrLn ("Error parsing configuration file " <> T.pack configPath)
            exitFailure
          Just conf -> func conf
  case command of
    Version -> do
      putDoc versionDoc
      exitSuccess
    Init addr host port from fromName fee dbPath clientPath clientConfigFile startingCycle cycleLength snapshotInterval -> do
      let config = Config addr host port from fromName [(startingCycle, fee)] dbPath Nothing clientPath clientConfigFile startingCycle cycleLength snapshotInterval Nothing Nothing Nothing
      writeConfig configPath config
      exitSuccess
    Monitor -> withConfig $ \config -> do
      event <- case configRiemann config of
                 Nothing -> return $ const $ return ()
                 Just (RiemannConfig host port) -> do
                   conn <- Riemann.tcpConnection (T.unpack host) port
                   return $ Riemann.sendEvents conn . Seq.singleton
      let conf  = RPC.Config (configHost config) (configPort config)
          baker = configBakerAddress config
          waitUntil height = do
            let helper prev = do
                  [head]:_ <- RPC.blocks conf
                  if Just head == prev then threadDelay (P.round (1e6 :: Double)) >> helper prev else do
                    header <- RPC.header conf head
                    event $ Riemann.ok "tezos" & Riemann.metric (RPC.headerLevel header) & Riemann.description ("Hash: " <> T.unpack (RPC.headerHash header)) & Riemann.ttl 360
                    if RPC.headerLevel header == height then return head else helper (Just head)
            helper Nothing
      [head]:_ <- RPC.blocks conf
      level <- RPC.currentLevel conf head
      let cycle = RPC.levelCycle level
      T.putStrLn $ T.concat ["Current cycle: ", T.pack $ P.show cycle]
      let next cycle = do
            T.putStrLn $ T.concat ["Scanning rights for cycle ", T.pack $ P.show cycle, "..."]
            baking <- filter ((==) 0 . RPC.bakingPriority) `fmap` RPC.bakingRightsFor conf head baker cycle
            endorsing <- RPC.endorsingRightsFor conf head baker cycle
            if not (null baking) || not (null endorsing) then return (cycle, baking, endorsing) else next (cycle + 1)
      (cycle, baking, endorsing) <- next cycle
      T.putStrLn $ T.concat ["Found rights in cycle ", T.pack $ P.show cycle, ": ", T.pack $ P.show $ P.length baking, " blocks to bake (priority 0), ",
        T.pack $ P.show $ P.length endorsing, " blocks to endorse."]
      let levelToWait (Right e) = RPC.endorsingLevel e + 1
          levelToWait (Left b)  = RPC.bakingLevel b
          allRights = sortBy (compare `on` levelToWait) $ filter (\x -> levelToWait x > RPC.levelLevel level) (fmap Right endorsing <> fmap Left baking)
      forM_ allRights $ \right -> do
        hash <- waitUntil (levelToWait right)
        case right of
          Right e -> do
            operations <- RPC.operations conf hash
            case P.filter ((==) (Just baker) . RPC.opmetadataDelegate . RPC.opcontentsMetadata . P.head . RPC.operationContents) operations of
              [] -> event $ Riemann.failure "endorser" & Riemann.metric (RPC.endorsingLevel e) & Riemann.description ("Hash: " <> T.unpack hash) & Riemann.ttl 86400
              _ -> event $ Riemann.ok "endorser" & Riemann.metric (RPC.endorsingLevel e) & Riemann.description ("Hash: " <> T.unpack hash) & Riemann.ttl 86400
          Left _ -> do
            metadata <- RPC.metadata conf hash
            if RPC.metadataBaker metadata == baker then
              event $ Riemann.ok "baker" & Riemann.metric (levelToWait right) & Riemann.description ("Hash: " <> P.show hash) & Riemann.ttl 86400
            else
              event $ Riemann.failure "baker" & Riemann.metric (levelToWait right) & Riemann.description ("Stolen by: " P.++ T.unpack (RPC.metadataBaker metadata)) & Riemann.ttl 86400
    Payout noDryRun continuous -> withConfig $ \config -> do
      notify <- case configTelegram config of
        Nothing -> return T.putStrLn
        Just (TelegramConfig token channelNotification) -> do
          env <- TG.defaultTelegramClientEnv (TG.Token token)
          return (\msg -> do
            _ <- TG.runClientM (TG.sendMessage (TG.SendMessageRequest (TG.SomeChatUsername channelNotification) msg Nothing Nothing Nothing Nothing Nothing)) env
            T.putStrLn $ T.concat ["Notified ", channelNotification, " with \"", msg, "\""])
      fromPassword <- do
        hSetEcho stdin False
        System.IO.putStr "Enter source account password: "
        hFlush stdout
        pass <- getLine
        putChar '\n'
        hSetEcho stdin True
        return pass
      payout config noDryRun (case P.length fromPassword of 0 -> Nothing; _ -> Just $ T.pack fromPassword) continuous notify

aboutDoc ∷ Doc
aboutDoc = mconcat [
  text "Bäckerei – Tooling for the Cryptium Tezos Bäckerei",
  line,
  text "© 2018-2019 Cryptium Labs • https://cryptium.ch"
  ]

versionDoc ∷ Doc
versionDoc = mconcat [
  aboutDoc,
  line,
  mconcat ["Prerelease version. This is alpha software.", line],
  mconcat ["Built from branch ", white $(gitBranch), " at commit ", red $(gitHash), ".", line]
  ]
