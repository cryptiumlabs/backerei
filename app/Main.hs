{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Control.Monad
import qualified Data.Aeson                   as A
import           Data.Function                (on)
import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Development.GitRev
import           Foundation
import           Options.Applicative
import qualified Prelude                      as P
import           System.Directory
import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import qualified Backerei.Delegation          as Delegation
import qualified Backerei.RPC                 as RPC
import qualified Backerei.Types               as RPC

import           Config
import           Options

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
    Init addr host port -> do
      let config = Config addr host port
      writeConfig configPath config
      exitSuccess
    Status -> withConfig $ \config -> do
      let conf  = RPC.Config (configHost config) (configPort config)
          baker = configBakerAddress config
      [[head]] <- RPC.blocks conf
      T.putStrLn $ T.concat ["Chain head: ", head]
      delegatedBalance <- RPC.delegatedBalance conf head baker
      T.putStrLn $ T.concat ["Delegated balance: ", delegatedBalance, " mutez"]
      frozenBalance <- RPC.frozenBalance conf head baker
      T.putStrLn $ T.concat ["Frozen balance: ", frozenBalance, " mutez"]
      stakingBalance <- RPC.stakingBalance conf head baker
      T.putStrLn $ T.concat ["Staking balance: ", stakingBalance, " mutez"]
      delegators <- RPC.delegatedContracts conf head baker
      T.putStrLn $ T.concat ["Delegators (", T.pack $ P.show (P.length delegators), "):"]
      mapM_ T.putStrLn delegators
      exitSuccess
    Monitor -> withConfig $ \config -> do
      let conf  = RPC.Config (configHost config) (configPort config)
          baker = configBakerAddress config
      [[head]] <- RPC.blocks conf
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
      let firstBaking:_ = sortBy (compare `on` RPC.bakingLevel) baking
          firstEndorsing:_ = sortBy (compare `on` RPC.endorsingLevel) endorsing
      T.putStrLn $ T.concat ["First baking right: ", T.pack $ P.show firstBaking]
      T.putStrLn $ T.concat ["First endorsing right: ", T.pack $ P.show firstEndorsing]
    Payout cycle -> withConfig $ \config -> do
      let conf  = RPC.Config (configHost config) (configPort config)
          baker = configBakerAddress config
      contributing <- Delegation.getContributingBalancesFor conf cycle baker
      mapM_ (\(x, y) -> T.putStrLn $ T.concat [x, " => ", y]) contributing

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
