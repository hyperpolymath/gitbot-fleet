-- SPDX-License-Identifier: AGPL-3.0-or-later
-- SPDX-FileCopyrightText: 2024-2025 hyperpolymath

{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Core metric types for Oikos Bot analysis
module Types.Metrics
  ( -- * Ecological Metrics
    EcoMetrics(..)
  , CarbonScore(..)
  , EnergyScore(..)
  , ResourceScore(..)

    -- * Economic Metrics
  , EconMetrics(..)
  , ParetoPoint(..)
  , ParetoFrontier(..)
  , AllocationScore(..)
  , DebtEstimate(..)

    -- * Quality Metrics
  , QualityMetrics(..)
  , ComplexityMetrics(..)
  , CouplingScore(..)
  , CoverageAnalysis(..)

    -- * Composite
  , HealthIndex(..)
  , AnalysisResult(..)
  ) where

import GHC.Generics (Generic)
import Data.Aeson (ToJSON, FromJSON)
import Data.Text (Text)

-- | Carbon intensity score based on SCI specification (ISO/IEC 21031:2024)
-- Score normalized to 0-100, where 100 is best (lowest carbon)
data CarbonScore = CarbonScore
  { carbonValue      :: !Double     -- ^ Raw carbon intensity estimate
  , carbonNormalized :: !Double     -- ^ Normalized 0-100 score
  , carbonFactors    :: ![Text]     -- ^ Contributing factors
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Energy efficiency patterns score
data EnergyScore = EnergyScore
  { energyPatterns    :: ![EnergyPattern]
  , energyNormalized  :: !Double
  , energyHotspots    :: ![CodeLocation]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Energy pattern classification
data EnergyPattern
  = BusyWaiting CodeLocation
  | IneffientLoop CodeLocation
  | BlockingIO CodeLocation
  | RedundantComputation CodeLocation
  | EfficientPattern CodeLocation
  deriving stock (Show, Eq, Generic)
  deriving anyclass (ToJSON, FromJSON)

-- | Code location reference
data CodeLocation = CodeLocation
  { locFile   :: !Text
  , locLine   :: !Int
  , locColumn :: !Int
  , locSnippet :: !(Maybe Text)
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Resource utilization score
data ResourceScore = ResourceScore
  { memoryEfficiency :: !Double
  , cpuEfficiency    :: !Double
  , ioEfficiency     :: !Double
  , resourceNormalized :: !Double
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Combined ecological metrics
data EcoMetrics = EcoMetrics
  { ecoCarbon   :: !CarbonScore
  , ecoEnergy   :: !EnergyScore
  , ecoResource :: !ResourceScore
  , ecoScore    :: !Double  -- ^ Weighted composite 0-100
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | A point in the Pareto frontier space
data ParetoPoint = ParetoPoint
  { ppDimensions :: ![Double]  -- ^ Values for each objective
  , ppLabels     :: ![Text]    -- ^ Objective names
  , ppDominated  :: !Bool      -- ^ Is this point dominated?
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | The Pareto frontier for multi-objective optimization
data ParetoFrontier = ParetoFrontier
  { pfPoints      :: ![ParetoPoint]
  , pfObjectives  :: ![Text]
  , pfCurrentPos  :: !ParetoPoint  -- ^ Current solution position
  , pfDistance    :: !Double       -- ^ Distance from frontier
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Allocative efficiency score
data AllocationScore = AllocationScore
  { allocEfficiency :: !Double     -- ^ 0-1 allocative efficiency
  , allocWaste      :: !Double     -- ^ Estimated resource waste
  , allocSuggestions :: ![Text]    -- ^ Improvement suggestions
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Technical debt estimation
data DebtEstimate = DebtEstimate
  { debtPrincipal  :: !Double      -- ^ Estimated hours to fix
  , debtInterest   :: !Double      -- ^ Ongoing maintenance cost
  , debtRatio      :: !Double      -- ^ Debt ratio (debt/value)
  , debtItems      :: ![DebtItem]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Individual technical debt item
data DebtItem = DebtItem
  { diLocation    :: !CodeLocation
  , diType        :: !Text
  , diSeverity    :: !Double
  , diDescription :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Combined economic metrics
data EconMetrics = EconMetrics
  { econPareto     :: !ParetoFrontier
  , econAllocation :: !AllocationScore
  , econDebt       :: !DebtEstimate
  , econScore      :: !Double  -- ^ Weighted composite 0-100
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Cyclomatic and cognitive complexity
data ComplexityMetrics = ComplexityMetrics
  { cmCyclomatic     :: !Int
  , cmCognitive      :: !Int
  , cmLinesOfCode    :: !Int
  , cmMaintainability :: !Double
  , cmHotspots       :: ![CodeLocation]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Coupling/cohesion analysis
data CouplingScore = CouplingScore
  { csAfferent   :: !Int      -- ^ Incoming dependencies
  , csEfferent   :: !Int      -- ^ Outgoing dependencies
  , csInstability :: !Double  -- ^ Instability metric
  , csAbstractness :: !Double -- ^ Abstractness metric
  , csDistance   :: !Double   -- ^ Distance from main sequence
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Test coverage analysis
data CoverageAnalysis = CoverageAnalysis
  { caLineCoverage     :: !Double
  , caBranchCoverage   :: !Double
  , caFunctionCoverage :: !Double
  , caUncoveredHotspots :: ![CodeLocation]
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Combined quality metrics
data QualityMetrics = QualityMetrics
  { qualComplexity :: !ComplexityMetrics
  , qualCoupling   :: !CouplingScore
  , qualCoverage   :: !(Maybe CoverageAnalysis)
  , qualScore      :: !Double  -- ^ Weighted composite 0-100
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Composite health index
data HealthIndex = HealthIndex
  { hiEco     :: !Double  -- ^ Ecological score weight
  , hiEcon    :: !Double  -- ^ Economic score weight
  , hiQuality :: !Double  -- ^ Quality score weight
  , hiTotal   :: !Double  -- ^ Weighted total 0-100
  , hiGrade   :: !Text    -- ^ A/B/C/D/F grade
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)

-- | Complete analysis result
data AnalysisResult = AnalysisResult
  { arEco       :: !EcoMetrics
  , arEcon      :: !EconMetrics
  , arQuality   :: !QualityMetrics
  , arHealth    :: !HealthIndex
  , arTimestamp :: !Text
  , arVersion   :: !Text
  } deriving stock (Show, Eq, Generic)
    deriving anyclass (ToJSON, FromJSON)
