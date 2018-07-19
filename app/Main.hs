{-# LANGUAGE TemplateHaskell #-}

module Main where

import           Development.GitRev
import           Foundation
import           Options.Applicative
import           System.Exit
import           Text.PrettyPrint.ANSI.Leijen hiding ((<>))

import           Options

main ∷ IO ()
main = do
  let opts = info (options <**> helper) (fullDesc <> headerDoc (Just aboutDoc))
  run =<< execParser opts

run ∷ Options → IO ()
run (Options cmd) = do
  case cmd of
    Version -> do
      putDoc versionDoc
      exitSuccess

aboutDoc ∷ Doc
aboutDoc = mconcat [
  text "Bäckerei – Tooling for the Cryptium Tezos Bäckerei",
  line,
  text "© 2018 Cryptium Labs • https://cryptium.ch",
  line
  ]

versionDoc ∷ Doc
versionDoc = mconcat [
  aboutDoc,
  line,
  mconcat ["Prerelease version. This is alpha software.", line],
  mconcat ["Built from branch ", white $(gitBranch), " at commit ", red $(gitHash), ".", line]
  ]
