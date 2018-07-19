{-# LANGUAGE TemplateHaskell #-}

module Main where

import qualified Data.Text                    as T
import qualified Data.Text.IO                 as T
import           Development.GitRev
import           Foundation
import           Options.Applicative
import           System.Directory
import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

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
run (Options configPath command host port) = do
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
    Init addr -> do
      let config = Config addr
      writeConfig configPath config
      exitSuccess
    Delegators -> withConfig $ \config -> do
      exitSuccess

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
