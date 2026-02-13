-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2024-2025 hyperpolymath

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE OverloadedStrings #-}

-- | Report types for Oikos Bot analysis output
module Types.Report
  ( -- * Report Types
    AnalysisReport(..)
  , ReportSection(..)
  , ReportItem(..)
  , Severity(..)
  , Recommendation(..)

    -- * Report Generation
  , emptyReport
  , addSection
  , addItem
  , toJSON
  , toMarkdown
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import qualified Data.Aeson as Aeson
import Data.Text (Text)
import qualified Data.Text as T
import Data.ByteString.Lazy (ByteString)
import Types.Metrics

-- | Severity level for findings
data Severity
  = Info      -- ^ Informational, no action needed
  | Warning   -- ^ Should be addressed eventually
  | Critical  -- ^ Must be addressed soon
  | Blocker   -- ^ Blocks deployment/merge
  deriving stock (Show, Eq, Ord, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | A recommendation for improvement
data Recommendation = Recommendation
  { recTitle       :: !Text
  , recDescription :: !Text
  , recSeverity    :: !Severity
  , recLocation    :: !(Maybe CodeLocation)
  , recEstimate    :: !(Maybe Double)  -- ^ Estimated hours to fix
  , recCategory    :: !Text            -- ^ eco, quality, security, etc.
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Individual report item (finding)
data ReportItem = ReportItem
  { riTitle       :: !Text
  , riDescription :: !Text
  , riSeverity    :: !Severity
  , riLocation    :: !(Maybe CodeLocation)
  , riRecommendations :: ![Recommendation]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A section of the report
data ReportSection = ReportSection
  { rsName     :: !Text
  , rsScore    :: !Double      -- ^ Section score 0-100
  , rsItems    :: ![ReportItem]
  , rsSummary  :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Complete analysis report
data AnalysisReport = AnalysisReport
  { arRepoName    :: !Text
  , arCommitSha   :: !Text
  , arBranch      :: !Text
  , arAnalyzedAt  :: !Text       -- ^ ISO 8601 timestamp
  , arDuration    :: !Double     -- ^ Analysis duration in seconds
  , arSections    :: ![ReportSection]
  , arMetrics     :: !AnalysisResult
  , arOverallScore :: !Double
  , arGrade       :: !Text       -- ^ A/B/C/D/F
  , arRecommendations :: ![Recommendation]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Create empty report
emptyReport :: Text -> Text -> Text -> AnalysisReport
emptyReport repoName commitSha branch = AnalysisReport
  { arRepoName = repoName
  , arCommitSha = commitSha
  , arBranch = branch
  , arAnalyzedAt = ""
  , arDuration = 0
  , arSections = []
  , arMetrics = defaultMetrics
  , arOverallScore = 0
  , arGrade = "F"
  , arRecommendations = []
  }
  where
    defaultMetrics = AnalysisResult
      { arEco = defaultEcoMetrics
      , arEcon = defaultEconMetrics
      , arQuality = defaultQualityMetrics
      , arHealth = HealthIndex 0 0 0 0 "F"
      , arTimestamp = ""
      , arVersion = "0.1.0"
      }

    defaultEcoMetrics = EcoMetrics
      { ecoCarbon = CarbonScore 0 0 []
      , ecoEnergy = EnergyScore [] 0 []
      , ecoResource = ResourceScore 0 0 0 0
      , ecoScore = 0
      }

    defaultEconMetrics = EconMetrics
      { econPareto = ParetoFrontier [] [] (ParetoPoint [] [] False) 0
      , econAllocation = AllocationScore 0 0 []
      , econDebt = DebtEstimate 0 0 0 []
      , econScore = 0
      }

    defaultQualityMetrics = QualityMetrics
      { qualComplexity = ComplexityMetrics 0 0 0 0 []
      , qualCoupling = CouplingScore 0 0 0 0 0
      , qualCoverage = Nothing
      , qualScore = 0
      }

-- | Add a section to the report
addSection :: ReportSection -> AnalysisReport -> AnalysisReport
addSection section report = report
  { arSections = arSections report ++ [section]
  }

-- | Add an item to a section
addItem :: Text -> ReportItem -> AnalysisReport -> AnalysisReport
addItem sectionName item report = report
  { arSections = map updateSection (arSections report)
  }
  where
    updateSection s
      | rsName s == sectionName = s { rsItems = rsItems s ++ [item] }
      | otherwise = s

-- | Convert report to JSON
toJSON :: AnalysisReport -> ByteString
toJSON = Aeson.encode

-- | Convert report to Markdown
toMarkdown :: AnalysisReport -> Text
toMarkdown report = T.unlines
  [ "# Oikos Bot Analysis Report"
  , ""
  , "## Summary"
  , ""
  , T.concat ["**Repository:** ", arRepoName report]
  , T.concat ["**Commit:** ", T.take 8 (arCommitSha report)]
  , T.concat ["**Branch:** ", arBranch report]
  , T.concat ["**Score:** ", T.pack (show (round (arOverallScore report) :: Int)), "/100"]
  , T.concat ["**Grade:** ", arGrade report]
  , ""
  , "## Sections"
  , ""
  , T.unlines (map sectionToMd (arSections report))
  , "## Recommendations"
  , ""
  , T.unlines (map recToMd (arRecommendations report))
  ]
  where
    sectionToMd s = T.unlines
      [ T.concat ["### ", rsName s, " (", T.pack (show (round (rsScore s) :: Int)), "/100)"]
      , ""
      , rsSummary s
      , ""
      , T.unlines (map itemToMd (rsItems s))
      ]

    itemToMd i = T.unlines
      [ T.concat ["- **", riTitle i, "** [", T.pack (show (riSeverity i)), "]"]
      , T.concat ["  ", riDescription i]
      ]

    recToMd r = T.unlines
      [ T.concat ["- **", recTitle r, "** [", T.pack (show (recSeverity r)), "]"]
      , T.concat ["  ", recDescription r]
      , maybe "" (\h -> T.concat ["  Estimate: ", T.pack (show h), " hours"]) (recEstimate r)
      ]
