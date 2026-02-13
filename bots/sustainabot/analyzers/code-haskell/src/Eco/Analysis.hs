{-# LANGUAGE OverloadedStrings #-}

-- | Main analysis orchestrator
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Coordinates all analysis modules and produces unified results.
module Eco.Analysis
  ( -- * Analysis Entry Points
    analyzeRepository
  , analyzeFile
  , analyzeDirectory

    -- * Configuration
  , AnalysisConfig(..)
  , defaultAnalysisConfig

    -- * Results
  , analysisToJSON
  , analysisToReport
  ) where

import Types.Metrics
import Types.Report
import Eco.Carbon
import Eco.Energy
import Eco.Pareto
import Eco.Resource
import Quality.Complexity
import Quality.Coupling
import Quality.Debt
import Quality.Coverage

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import qualified Data.Aeson as Aeson
import Data.ByteString.Lazy (ByteString)
import System.Directory (listDirectory, doesFileExist, doesDirectoryExist)
import System.FilePath ((</>), takeExtension)
import Control.Monad (filterM, forM)
import Data.Time.Clock (getCurrentTime)
import Data.Time.Format (formatTime, defaultTimeLocale)

-- | Configuration for the analysis engine
data AnalysisConfig = AnalysisConfig
  { acCarbonConfig     :: !CarbonConfig
  , acEnergyConfig     :: !EnergyConfig
  , acResourceConfig   :: !ResourceConfig
  , acComplexityConfig :: !ComplexityConfig
  , acCouplingConfig   :: !CouplingConfig
  , acDebtConfig       :: !DebtConfig
  , acCoverageConfig   :: !CoverageConfig
  , acEcoWeight        :: !Double         -- ^ Weight for ecological score
  , acEconWeight       :: !Double         -- ^ Weight for economic score
  , acQualityWeight    :: !Double         -- ^ Weight for quality score
  , acExcludePatterns  :: ![Text]         -- ^ Patterns to exclude
  , acCoverageFile     :: !(Maybe FilePath)  -- ^ Path to coverage report
  } deriving (Show, Eq)

-- | Default analysis configuration
defaultAnalysisConfig :: AnalysisConfig
defaultAnalysisConfig = AnalysisConfig
  { acCarbonConfig = defaultCarbonConfig
  , acEnergyConfig = defaultEnergyConfig
  , acResourceConfig = defaultResourceConfig
  , acComplexityConfig = defaultComplexityConfig
  , acCouplingConfig = defaultCouplingConfig
  , acDebtConfig = defaultDebtConfig
  , acCoverageConfig = defaultCoverageConfig
  , acEcoWeight = 0.33
  , acEconWeight = 0.33
  , acQualityWeight = 0.34
  , acExcludePatterns = ["node_modules", "vendor", ".git", "dist", "build"]
  , acCoverageFile = Nothing
  }

-- | Analyze an entire repository
analyzeRepository :: AnalysisConfig -> FilePath -> IO AnalysisResult
analyzeRepository config repoPath = do
  -- Find all source files
  files <- findSourceFiles config repoPath

  -- Analyze each file
  fileResults <- mapM (analyzeFile config) files

  -- Aggregate results
  let ecoMetrics = aggregateEcoMetrics (acCarbonConfig config) (acEnergyConfig config) fileResults
  let qualMetrics = aggregateQualityMetrics fileResults

  -- Calculate economic metrics (Pareto, debt, allocation)
  let econMetrics = calculateEconMetrics config qualMetrics

  -- Calculate health index
  let healthIndex = calculateHealthIndex config ecoMetrics econMetrics qualMetrics

  -- Get timestamp
  now <- getCurrentTime
  let timestamp = T.pack $ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M:%SZ" now

  pure AnalysisResult
    { arEco = ecoMetrics
    , arEcon = econMetrics
    , arQuality = qualMetrics
    , arHealth = healthIndex
    , arTimestamp = timestamp
    , arVersion = "0.1.0"
    }

-- | Analyze a single file
analyzeFile :: AnalysisConfig -> FilePath -> IO FileAnalysisResult
analyzeFile config filePath = do
  content <- TIO.readFile filePath
  pure $ analyzeContent config (T.pack filePath) content

-- | Internal file analysis result
data FileAnalysisResult = FileAnalysisResult
  { farPath       :: !Text
  , farLines      :: !Int
  , farComplexity :: !Int
  , farPatterns   :: ![EnergyPattern]
  , farIssues     :: ![DebtItem]
  } deriving (Show, Eq)

-- | Analyze file content
analyzeContent :: AnalysisConfig -> Text -> Text -> FileAnalysisResult
analyzeContent _config path content = FileAnalysisResult
  { farPath = path
  , farLines = length $ T.lines content
  , farComplexity = estimateComplexity content
  , farPatterns = detectPatterns content
  , farIssues = detectIssues path content
  }

-- | Estimate cyclomatic complexity from content
estimateComplexity :: Text -> Int
estimateComplexity content =
  1 + branchKeywords + loopKeywords
  where
    countOccurrences needle haystack =
      length $ T.breakOnAll needle haystack

    branchKeywords = sum
      [ countOccurrences "if " content
      , countOccurrences "if(" content
      , countOccurrences "else " content
      , countOccurrences "case " content
      , countOccurrences "?" content  -- ternary
      ]

    loopKeywords = sum
      [ countOccurrences "for " content
      , countOccurrences "for(" content
      , countOccurrences "while " content
      , countOccurrences "while(" content
      ]

-- | Detect energy patterns in content
detectPatterns :: Text -> [EnergyPattern]
detectPatterns content = concat
  [ busyWaits
  , inefficientLoops
  ]
  where
    busyWaits =
      [ BusyWaiting (CodeLocation "" 0 0 (Just "Potential busy wait"))
      | "while(true)" `T.isInfixOf` content || "while (true)" `T.isInfixOf` content
      ]

    inefficientLoops =
      [ IneffientLoop (CodeLocation "" 0 0 (Just "Nested loop detected"))
      | hasNestedLoops content
      ]

    hasNestedLoops c =
      let lines' = T.lines c
          loopLines = filter isLoopLine lines'
      in length loopLines > 3

    isLoopLine l = "for " `T.isInfixOf` l || "while " `T.isInfixOf` l

-- | Detect code issues
detectIssues :: Text -> Text -> [DebtItem]
detectIssues path content = concat
  [ todoIssues
  , longFunctionIssues
  ]
  where
    todoIssues =
      [ DebtItem
          { diLocation = CodeLocation path lineNum 1 (Just $ T.take 50 line)
          , diType = "TODO"
          , diSeverity = 3
          , diDescription = "TODO comment found"
          }
      | (lineNum, line) <- zip [1..] (T.lines content)
      , "TODO" `T.isInfixOf` line || "FIXME" `T.isInfixOf` line
      ]

    longFunctionIssues =
      [ DebtItem
          { diLocation = CodeLocation path 1 1 Nothing
          , diType = "LongFile"
          , diSeverity = 5
          , diDescription = "File exceeds 500 lines"
          }
      | length (T.lines content) > 500
      ]

-- | Analyze a directory
analyzeDirectory :: AnalysisConfig -> FilePath -> IO AnalysisResult
analyzeDirectory = analyzeRepository

-- | Find source files in a directory
findSourceFiles :: AnalysisConfig -> FilePath -> IO [FilePath]
findSourceFiles config rootPath = do
  exists <- doesDirectoryExist rootPath
  if not exists
    then pure []
    else findFiles rootPath
  where
    findFiles dir = do
      entries <- listDirectory dir
      let fullPaths = map (dir </>) entries
          excluded = any (\p -> T.pack p `T.isInfixOf` T.pack dir) (acExcludePatterns config)
      if excluded
        then pure []
        else do
          files <- filterM doesFileExist fullPaths
          dirs <- filterM doesDirectoryExist fullPaths
          subFiles <- concat <$> mapM findFiles dirs
          let sourceFiles = filter isSourceFile files
          pure $ sourceFiles ++ subFiles

    isSourceFile f = takeExtension f `elem`
      [".hs", ".rs", ".res", ".ml", ".js", ".ts", ".py", ".go", ".java", ".rb"]

-- | Aggregate eco metrics from file results
aggregateEcoMetrics :: CarbonConfig -> EnergyConfig -> [FileAnalysisResult] -> EcoMetrics
aggregateEcoMetrics carbonConfig _energyConfig results = EcoMetrics
  { ecoCarbon = carbonScore
  , ecoEnergy = energyScore
  , ecoResource = resourceScore
  , ecoScore = (carbonNormalized carbonScore + energyNormalized energyScore + resourceNormalized resourceScore) / 3
  }
  where
    totalComplexity = sum $ map farComplexity results
    totalLines = sum $ map farLines results
    allPatterns = concatMap farPatterns results

    carbonInput = CodeAnalysisInput
      { caiComplexity = totalComplexity
      , caiLoopDepth = 2  -- Estimated
      , caiAllocations = totalLines `div` 10
      , caiIOOperations = length results
      , caiParallelism = 1
      }

    carbonScore = analyzeCarbonIntensity carbonConfig carbonInput

    energyScore = EnergyScore
      { energyPatterns = allPatterns
      , energyNormalized = max 0 $ 100 - fromIntegral (length allPatterns) * 10
      , energyHotspots = []
      }

    resourceScore = ResourceScore
      { memoryEfficiency = 80
      , cpuEfficiency = 85
      , ioEfficiency = 90
      , resourceNormalized = 85
      }

-- | Aggregate quality metrics from file results
aggregateQualityMetrics :: [FileAnalysisResult] -> QualityMetrics
aggregateQualityMetrics results = QualityMetrics
  { qualComplexity = ComplexityMetrics
      { cmCyclomatic = maxComplexity
      , cmCognitive = maxComplexity  -- Simplified
      , cmLinesOfCode = totalLines
      , cmMaintainability = maintainability
      , cmHotspots = []
      }
  , qualCoupling = CouplingScore 5 10 0.5 0.3 0.2
  , qualCoverage = Nothing
  , qualScore = maintainability
  }
  where
    totalLines = sum $ map farLines results
    maxComplexity = if null results then 0 else maximum $ map farComplexity results
    avgComplexity = if null results then 0 else sum (map farComplexity results) `div` length results
    maintainability = max 0 $ 100 - fromIntegral avgComplexity * 2

-- | Calculate economic metrics
calculateEconMetrics :: AnalysisConfig -> QualityMetrics -> EconMetrics
calculateEconMetrics config qualMetrics = EconMetrics
  { econPareto = paretoFrontier
  , econAllocation = allocationScore
  , econDebt = debtEstimate
  , econScore = (100 - debtRatio debtEstimate * 100 + allocEfficiency allocationScore * 100) / 2
  }
  where
    -- Simplified Pareto analysis
    paretoFrontier = calculateParetoFrontier standardObjectives
      [ [qualScore qualMetrics, fromIntegral $ cmCyclomatic $ qualComplexity qualMetrics]
      ]

    allocationScore = AllocationScore
      { allocEfficiency = 0.7
      , allocWaste = 0.2
      , allocSuggestions = ["Reduce code duplication", "Improve test coverage"]
      }

    debtEstimate = DebtEstimate
      { debtPrincipal = fromIntegral (cmCyclomatic $ qualComplexity qualMetrics) * 2
      , debtInterest = fromIntegral (cmCyclomatic $ qualComplexity qualMetrics) * 0.3
      , debtRatio = 0.1
      , debtItems = []
      }

-- | Calculate overall health index
calculateHealthIndex :: AnalysisConfig -> EcoMetrics -> EconMetrics -> QualityMetrics -> HealthIndex
calculateHealthIndex config eco econ qual = HealthIndex
  { hiEco = acEcoWeight config
  , hiEcon = acEconWeight config
  , hiQuality = acQualityWeight config
  , hiTotal = totalScore
  , hiGrade = toGrade totalScore
  }
  where
    totalScore = acEcoWeight config * ecoScore eco
              + acEconWeight config * econScore econ
              + acQualityWeight config * qualScore qual

    toGrade score
      | score >= 90 = "A"
      | score >= 80 = "B"
      | score >= 70 = "C"
      | score >= 60 = "D"
      | otherwise = "F"

-- | Convert analysis result to JSON
analysisToJSON :: AnalysisResult -> ByteString
analysisToJSON = Aeson.encode

-- | Convert analysis result to report
analysisToReport :: Text -> Text -> Text -> AnalysisResult -> AnalysisReport
analysisToReport repoName commitSha branch result =
  (emptyReport repoName commitSha branch)
    { arMetrics = result
    , arOverallScore = hiTotal $ arHealth result
    , arGrade = hiGrade $ arHealth result
    , arSections =
        [ ReportSection "Ecological" (ecoScore $ arEco result) [] "Carbon and energy analysis"
        , ReportSection "Economic" (econScore $ arEcon result) [] "Pareto and debt analysis"
        , ReportSection "Quality" (qualScore $ arQuality result) [] "Complexity and coverage analysis"
        ]
    }
