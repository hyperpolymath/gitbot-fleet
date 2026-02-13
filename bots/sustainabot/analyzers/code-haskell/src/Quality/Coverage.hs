{-# LANGUAGE OverloadedStrings #-}

-- | Test coverage analysis
-- SPDX-License-Identifier: AGPL-3.0-or-later
--
-- Analyzes test coverage data and identifies uncovered hotspots.
module Quality.Coverage
  ( analyzeCoverage
  , parseCoverageReport
  , identifyUncoveredHotspots
  , CoverageConfig(..)
  , defaultCoverageConfig
  , CoverageFormat(..)
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for coverage analysis
data CoverageConfig = CoverageConfig
  { covMinLine     :: !Double      -- ^ Minimum acceptable line coverage
  , covMinBranch   :: !Double      -- ^ Minimum acceptable branch coverage
  , covMinFunction :: !Double      -- ^ Minimum acceptable function coverage
  , covHotspotThreshold :: !Int    -- ^ Lines of uncovered code to flag
  } deriving (Show, Eq)

-- | Default coverage thresholds
defaultCoverageConfig :: CoverageConfig
defaultCoverageConfig = CoverageConfig
  { covMinLine = 80.0
  , covMinBranch = 70.0
  , covMinFunction = 90.0
  , covHotspotThreshold = 10
  }

-- | Supported coverage report formats
data CoverageFormat
  = Lcov           -- ^ LCOV format
  | Cobertura      -- ^ Cobertura XML
  | JaCoCo         -- ^ JaCoCo XML
  | SimpleCov      -- ^ Ruby SimpleCov JSON
  | CoverageJSON   -- ^ Generic JSON format
  deriving (Show, Eq)

-- | Coverage data for a single file
data FileCoverage = FileCoverage
  { fcPath            :: !Text
  , fcTotalLines      :: !Int
  , fcCoveredLines    :: !Int
  , fcTotalBranches   :: !Int
  , fcCoveredBranches :: !Int
  , fcTotalFunctions  :: !Int
  , fcCoveredFunctions :: !Int
  , fcUncoveredRanges :: ![(Int, Int)]  -- ^ (start, end) line ranges
  } deriving (Show, Eq)

-- | Aggregate coverage data
data CoverageData = CoverageData
  { cdFiles         :: ![FileCoverage]
  , cdTotalLines    :: !Int
  , cdCoveredLines  :: !Int
  , cdTotalBranches :: !Int
  , cdCoveredBranches :: !Int
  , cdTotalFunctions :: !Int
  , cdCoveredFunctions :: !Int
  } deriving (Show, Eq)

-- | Analyze coverage data
analyzeCoverage :: CoverageConfig -> CoverageData -> CoverageAnalysis
analyzeCoverage config coverage = CoverageAnalysis
  { caLineCoverage = lineCov
  , caBranchCoverage = branchCov
  , caFunctionCoverage = funcCov
  , caUncoveredHotspots = hotspots
  }
  where
    lineCov = percentage (cdCoveredLines coverage) (cdTotalLines coverage)
    branchCov = percentage (cdCoveredBranches coverage) (cdTotalBranches coverage)
    funcCov = percentage (cdCoveredFunctions coverage) (cdTotalFunctions coverage)

    hotspots = identifyUncoveredHotspots config (cdFiles coverage)

    percentage num denom
      | denom == 0 = 100.0
      | otherwise = 100.0 * fromIntegral num / fromIntegral denom

-- | Identify significant uncovered code regions
identifyUncoveredHotspots :: CoverageConfig -> [FileCoverage] -> [CodeLocation]
identifyUncoveredHotspots config files =
  concatMap findHotspots files
  where
    findHotspots fc =
      [ CodeLocation
        { locFile = fcPath fc
        , locLine = startLine
        , locColumn = 1
        , locSnippet = Just $ T.concat ["Lines ", T.pack (show startLine), "-", T.pack (show endLine)]
        }
      | (startLine, endLine) <- fcUncoveredRanges fc
      , endLine - startLine >= covHotspotThreshold config
      ]

-- | Parse coverage report (placeholder - would use aeson in real impl)
parseCoverageReport :: CoverageFormat -> Text -> Either Text CoverageData
parseCoverageReport format _content =
  case format of
    Lcov -> Right emptyCoverage
    Cobertura -> Right emptyCoverage
    JaCoCo -> Right emptyCoverage
    SimpleCov -> Right emptyCoverage
    CoverageJSON -> Right emptyCoverage
  where
    emptyCoverage = CoverageData
      { cdFiles = []
      , cdTotalLines = 0
      , cdCoveredLines = 0
      , cdTotalBranches = 0
      , cdCoveredBranches = 0
      , cdTotalFunctions = 0
      , cdCoveredFunctions = 0
      }

-- | Suggest coverage improvements
suggestCoverageImprovements :: CoverageConfig -> CoverageAnalysis -> [Text]
suggestCoverageImprovements config analysis = concat
  [ lineSuggestions
  , branchSuggestions
  , functionSuggestions
  , hotspotSuggestions
  ]
  where
    lineSuggestions =
      [ T.concat ["Line coverage (", T.pack (show $ round $ caLineCoverage analysis :: Int),
                  "%) below threshold (", T.pack (show $ round $ covMinLine config :: Int), "%)"]
      | caLineCoverage analysis < covMinLine config
      ]

    branchSuggestions =
      [ T.concat ["Branch coverage (", T.pack (show $ round $ caBranchCoverage analysis :: Int),
                  "%) below threshold (", T.pack (show $ round $ covMinBranch config :: Int), "%)"]
      | caBranchCoverage analysis < covMinBranch config
      ]

    functionSuggestions =
      [ T.concat ["Function coverage (", T.pack (show $ round $ caFunctionCoverage analysis :: Int),
                  "%) below threshold (", T.pack (show $ round $ covMinFunction config :: Int), "%)"]
      | caFunctionCoverage analysis < covMinFunction config
      ]

    hotspotSuggestions =
      [ T.concat ["Add tests for uncovered region: ", maybe "" id $ locSnippet loc,
                  " in ", locFile loc]
      | loc <- take 5 $ caUncoveredHotspots analysis
      ]

-- | Calculate coverage score (0-100)
calculateCoverageScore :: CoverageConfig -> CoverageAnalysis -> Double
calculateCoverageScore config analysis =
  weightedAvg
    [ (caLineCoverage analysis, 0.5)
    , (caBranchCoverage analysis, 0.3)
    , (caFunctionCoverage analysis, 0.2)
    ]
  where
    weightedAvg pairs = sum [v * w | (v, w) <- pairs] / sum [w | (_, w) <- pairs]
