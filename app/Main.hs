{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Concurrent
import           Control.Monad
import           Data.Function                (on)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Development.GitRev
import           Foundation
import           Options.Applicative
import qualified Prelude                      as P
import qualified Servant.Client               as TG
import           System.Directory
import           System.Exit
import qualified Telegram.Bot.API             as TG
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import qualified Backerei.RPC                 as RPC
import qualified Backerei.Types               as RPC

import           Config
import           Options
import           Payout

main ∷ IO ()
main = do
  ctx <- context
  let opts = info (options ctx <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run =<< execParser opts

context ∷ IO Context
context = do
  home <- getHomeDirectory
  return (Context home)

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
    Init addr host port from fee dbPath clientPath clientConfigFile startingCycle cycleLength snapshotInterval -> do
      let config = Config addr host port from fee dbPath Nothing clientPath clientConfigFile startingCycle cycleLength snapshotInterval Nothing
      writeConfig configPath config
      exitSuccess
    Status -> withConfig $ \config -> do
      let conf  = RPC.Config (configHost config) (configPort config)
          baker = configBakerAddress config
      delegatedBalance <- RPC.delegatedBalanceAt conf RPC.head baker
      T.putStrLn $ T.concat ["Delegated balance: ", T.pack $ P.show delegatedBalance, " XTZ"]
      frozenBalance <- RPC.frozenBalanceAt conf RPC.head baker
      T.putStrLn $ T.concat ["Frozen balance: ", T.pack $ P.show frozenBalance, " XTZ"]
      stakingBalance <- RPC.stakingBalanceAt conf RPC.head baker
      T.putStrLn $ T.concat ["Staking balance: ", T.pack $ P.show stakingBalance, " XTZ"]
      delegators <- RPC.delegatedContracts conf RPC.head baker
      T.putStrLn $ T.concat ["Delegators (", T.pack $ P.show (P.length delegators), "):"]
      mapM_ T.putStrLn delegators
      exitSuccess
    Monitor -> withConfig $ \config -> do
      let conf  = RPC.Config (configHost config) (configPort config)
          baker = configBakerAddress config
          waitUntil height = do
            let helper prev = do
                  [head]:_ <- RPC.blocks conf
                  if Just head == prev then threadDelay (P.round (1e6 :: Double)) >> helper prev else do
                    header <- RPC.header conf head
                    T.putStrLn $ T.concat ["Current height: ", T.pack $ P.show $ RPC.headerLevel header]
                    if RPC.headerLevel header == height then return head else do
                      helper (Just head)
            T.putStrLn $ T.concat ["Waiting for height: ", T.pack $ P.show height]
            helper Nothing
      sendMessage <-  case configTelegram config of
                        Nothing -> return T.putStrLn
                        Just (TelegramConfig token channel) -> do
                          env <- TG.defaultTelegramClientEnv (TG.Token token)
                          return $ \msg -> do
                            _ <- TG.runClientM (TG.sendMessage (TG.SendMessageRequest (TG.SomeChatUsername channel) msg Nothing Nothing Nothing Nothing Nothing)) env
                            T.putStrLn msg
      [head]:_ <- RPC.blocks conf
      level <- RPC.currentLevel conf head
      let cycle = RPC.levelCycle level
      T.putStrLn $ T.concat ["Current cycle: ", T.pack $ P.show cycle]
      let next cycle = do
            T.putStrLn $ T.concat ["Scanning rights for cycle ", T.pack $ P.show cycle, "..."]
            baking <- filter ((==) 0 . RPC.bakingPriority) `fmap` RPC.bakingRightsFor conf head baker cycle
            endorsing <- RPC.endorsingRightsFor conf head baker cycle
            if length baking > 0 || length endorsing > 0 then return (cycle, baking, endorsing) else next (cycle + 1)
      (cycle, baking, endorsing) <- next cycle
      T.putStrLn $ T.concat ["Found rights in cycle ", T.pack $ P.show cycle, ": ", T.pack $ P.show $ P.length baking, " blocks to bake (priority 0), ",
        T.pack $ P.show $ P.length endorsing, " blocks to endorse."]
      let levelToWait (Right e) = RPC.endorsingLevel e + 1
          levelToWait (Left b)  = RPC.bakingLevel b
          allRights = sortBy (compare `on` levelToWait) $ filter (\x -> levelToWait x > RPC.levelLevel level) $ (fmap Right endorsing <> fmap Left baking)
      forM_ allRights $ \right -> do
        sendMessage $ T.concat ["Next baking/endorsing right: ", T.pack $ P.show right]
        hash <- waitUntil (levelToWait right)
        case right of
          Right e -> do
            operations <- RPC.operations conf hash
            case P.filter ((==) (Just baker) . RPC.opmetadataDelegate . RPC.opcontentsMetadata . P.head . RPC.operationContents) operations of
              [] -> sendMessage $ T.concat ["@cwgoes @adrianbrink Expected to endorse block ", T.pack $ P.show (RPC.endorsingLevel e), " but did not."]
              _ -> sendMessage $ T.concat ["Endorsement of block at height ", T.pack $ P.show $ RPC.endorsingLevel e, " OK!"]
          Left _ -> do
            metadata <- RPC.metadata conf hash
            if RPC.metadataBaker metadata == baker then do
              sendMessage $ T.concat ["Baked block ", T.pack $ P.show hash, " OK!"]
            else do
              sendMessage $ T.concat ["@cwgoes @adrianbrink Expected to bake but did not, instead baker was: ", RPC.metadataBaker metadata]
    Payout noDryRun -> withConfig $ flip payout noDryRun

aboutDoc ∷ Doc
aboutDoc = mconcat [
  text "Bäckerei – Tooling for the Cryptium Tezos Bäckerei",
  line,
  text "© 2018 Cryptium Labs • https://cryptium.ch"
  ]

versionDoc ∷ Doc
versionDoc = mconcat [
  aboutDoc,
  line,
  mconcat ["Prerelease version. This is alpha software.", line],
  mconcat ["Built from branch ", white $(gitBranch), " at commit ", red $(gitHash), ".", line]
  ]
