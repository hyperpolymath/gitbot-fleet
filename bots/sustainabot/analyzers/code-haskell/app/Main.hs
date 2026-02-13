{-# LANGUAGE OverloadedStrings #-}

-- | Eco-Analyzer CLI and HTTP Server
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Entry point for the Haskell code analyzer.
-- Supports both CLI analysis and HTTP server mode.
module Main where

import Eco.Analysis
import Types.Metrics
import Types.Report

import Options.Applicative
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.ByteString.Lazy as BL
import System.Exit (exitFailure, exitSuccess)
import Control.Monad (when)

-- | Command line options
data Options = Options
  { optMode      :: !Mode
  , optPath      :: !FilePath
  , optOutput    :: !OutputFormat
  , optVerbose   :: !Bool
  , optPort      :: !Int
  , optRepoName  :: !(Maybe Text)
  , optCommitSha :: !(Maybe Text)
  , optBranch    :: !(Maybe Text)
  } deriving (Show, Eq)

-- | Execution mode
data Mode
  = Analyze      -- ^ Analyze a repository/directory
  | Server       -- ^ Run as HTTP server
  | Version      -- ^ Show version
  deriving (Show, Eq)

-- | Output format
data OutputFormat
  = JSON         -- ^ JSON output
  | Markdown     -- ^ Markdown report
  | Summary      -- ^ Brief summary
  deriving (Show, Eq)

-- | Parse command line options
optionsParser :: Parser Options
optionsParser = Options
  <$> modeParser
  <*> strOption
      ( long "path"
     <> short 'p'
     <> metavar "PATH"
     <> value "."
     <> help "Path to analyze (default: current directory)" )
  <*> outputParser
  <*> switch
      ( long "verbose"
     <> short 'v'
     <> help "Verbose output" )
  <*> option auto
      ( long "port"
     <> metavar "PORT"
     <> value 8080
     <> help "HTTP server port (default: 8080)" )
  <*> optional (strOption
      ( long "repo"
     <> metavar "NAME"
     <> help "Repository name for report" ))
  <*> optional (strOption
      ( long "commit"
     <> metavar "SHA"
     <> help "Commit SHA for report" ))
  <*> optional (strOption
      ( long "branch"
     <> metavar "BRANCH"
     <> help "Branch name for report" ))

modeParser :: Parser Mode
modeParser = flag' Server
    ( long "server"
   <> short 's'
   <> help "Run as HTTP server" )
  <|> flag' Version
    ( long "version"
   <> help "Show version" )
  <|> pure Analyze

outputParser :: Parser OutputFormat
outputParser = flag' JSON
    ( long "json"
   <> short 'j'
   <> help "Output as JSON" )
  <|> flag' Markdown
    ( long "markdown"
   <> short 'm'
   <> help "Output as Markdown" )
  <|> pure Summary

-- | Main entry point
main :: IO ()
main = do
  opts <- execParser parserInfo
  case optMode opts of
    Version -> printVersion
    Server -> runServer opts
    Analyze -> runAnalysis opts

  where
    parserInfo = info (optionsParser <**> helper)
      ( fullDesc
     <> progDesc "Analyze code for ecological and economic metrics"
     <> header "eco-analyzer - Ecological & Economic Code Analysis" )

-- | Print version information
printVersion :: IO ()
printVersion = do
  putStrLn "eco-analyzer 0.1.0"
  putStrLn "Copyright (c) 2024 Hyperpolymath"
  putStrLn "License: AGPL-3.0-or-later"
  exitSuccess

-- | Run analysis on a path
runAnalysis :: Options -> IO ()
runAnalysis opts = do
  when (optVerbose opts) $
    putStrLn $ "Analyzing: " ++ optPath opts

  result <- analyzeRepository defaultAnalysisConfig (optPath opts)

  case optOutput opts of
    JSON -> BL.putStr $ analysisToJSON result
    Markdown -> do
      let report = analysisToReport
            (maybe "unknown" id $ optRepoName opts)
            (maybe "HEAD" id $ optCommitSha opts)
            (maybe "main" id $ optBranch opts)
            result
      TIO.putStrLn $ toMarkdown report
    Summary -> printSummary result

  -- Exit with appropriate code based on grade
  let grade = hiGrade $ arHealth result
  case grade of
    "A" -> exitSuccess
    "B" -> exitSuccess
    "C" -> exitSuccess
    _   -> exitFailure

-- | Print analysis summary
printSummary :: AnalysisResult -> IO ()
printSummary result = do
  let health = arHealth result
  putStrLn "═══════════════════════════════════════════"
  putStrLn "        ECO-ANALYZER SUMMARY"
  putStrLn "═══════════════════════════════════════════"
  putStrLn ""
  putStrLn $ "Overall Score: " ++ show (round $ hiTotal health :: Int) ++ "/100"
  putStrLn $ "Grade: " ++ T.unpack (hiGrade health)
  putStrLn ""
  putStrLn "───────────────────────────────────────────"
  putStrLn "Breakdown:"
  putStrLn $ "  Ecological:  " ++ show (round $ ecoScore $ arEco result :: Int) ++ "/100"
  putStrLn $ "  Economic:    " ++ show (round $ econScore $ arEcon result :: Int) ++ "/100"
  putStrLn $ "  Quality:     " ++ show (round $ qualScore $ arQuality result :: Int) ++ "/100"
  putStrLn ""
  putStrLn "───────────────────────────────────────────"
  putStrLn "Key Metrics:"
  putStrLn $ "  Carbon Score:     " ++ show (round $ carbonNormalized $ ecoCarbon $ arEco result :: Int)
  putStrLn $ "  Energy Patterns:  " ++ show (length $ energyPatterns $ ecoEnergy $ arEco result)
  putStrLn $ "  Cyclomatic:       " ++ show (cmCyclomatic $ qualComplexity $ arQuality result)
  putStrLn $ "  Technical Debt:   " ++ show (round $ debtPrincipal $ econDebt $ arEcon result :: Int) ++ "h"
  putStrLn ""
  putStrLn $ "Analysis version: " ++ T.unpack (arVersion result)
  putStrLn $ "Timestamp: " ++ T.unpack (arTimestamp result)
  putStrLn "═══════════════════════════════════════════"

-- | Run HTTP server mode
runServer :: Options -> IO ()
runServer opts = do
  putStrLn $ "Starting eco-analyzer server on port " ++ show (optPort opts)
  putStrLn "Endpoints:"
  putStrLn "  POST /analyze - Analyze repository"
  putStrLn "  GET  /health  - Health check"
  putStrLn ""
  putStrLn "Server mode not yet implemented."
  putStrLn "Use CLI mode: eco-analyzer --path /path/to/repo"
  -- TODO: Implement HTTP server using warp
  exitSuccess
