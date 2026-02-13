{-# LANGUAGE OverloadedStrings #-}

-- | Carbon intensity analysis based on SCI specification
-- Reference: ISO/IEC 21031:2024 (Software Carbon Intensity)
module Eco.Carbon
  ( analyzeCarbonIntensity
  , estimateOperationalCarbon
  , estimateEmbodiedCarbon
  , CarbonConfig(..)
  , defaultCarbonConfig
  ) where

import Types.Metrics
import Data.Text (Text)
import qualified Data.Text as T

-- | Configuration for carbon analysis
data CarbonConfig = CarbonConfig
  { ccGridIntensity   :: !Double  -- ^ gCO2eq/kWh for electricity grid
  , ccHardwareLifespan :: !Double -- ^ Expected hardware lifespan in years
  , ccUsagePercentage :: !Double  -- ^ % of hardware dedicated to this software
  } deriving (Show, Eq)

-- | Default configuration (global average)
defaultCarbonConfig :: CarbonConfig
defaultCarbonConfig = CarbonConfig
  { ccGridIntensity = 475     -- Global average gCO2eq/kWh
  , ccHardwareLifespan = 4    -- 4 year hardware lifecycle
  , ccUsagePercentage = 0.01  -- 1% of hardware for this workload
  }

-- | Analyze carbon intensity of code
--
-- Based on SCI formula: SCI = ((E * I) + M) / R
-- Where:
--   E = Energy consumed by software
--   I = Location-based marginal carbon intensity
--   M = Embodied emissions of hardware
--   R = Functional unit (per request, per user, etc.)
analyzeCarbonIntensity :: CarbonConfig -> CodeAnalysisInput -> CarbonScore
analyzeCarbonIntensity config input = CarbonScore
  { carbonValue = sciScore
  , carbonNormalized = normalizeScore sciScore
  , carbonFactors = identifyFactors input
  }
  where
    operationalCarbon = estimateOperationalCarbon config input
    embodiedCarbon = estimateEmbodiedCarbon config input
    sciScore = operationalCarbon + embodiedCarbon

-- | Estimate operational carbon (E * I from SCI)
estimateOperationalCarbon :: CarbonConfig -> CodeAnalysisInput -> Double
estimateOperationalCarbon config input =
  energyEstimate * (ccGridIntensity config / 1000)  -- Convert to kgCO2eq
  where
    energyEstimate = estimateEnergyConsumption input

-- | Estimate embodied carbon (M from SCI)
estimateEmbodiedCarbon :: CarbonConfig -> CodeAnalysisInput -> Double
estimateEmbodiedCarbon config _input =
  -- Simplified model: hardware manufacturing emissions amortized
  (hardwareEmissions * ccUsagePercentage config) / ccHardwareLifespan config
  where
    hardwareEmissions = 300  -- kg CO2eq for typical server (simplified)

-- | Placeholder for code analysis input
data CodeAnalysisInput = CodeAnalysisInput
  { caiComplexity    :: !Int        -- ^ Cyclomatic complexity
  , caiLoopDepth     :: !Int        -- ^ Maximum loop nesting depth
  , caiAllocations   :: !Int        -- ^ Number of heap allocations
  , caiIOOperations  :: !Int        -- ^ Number of I/O operations
  , caiParallelism   :: !Int        -- ^ Degree of parallelism
  } deriving (Show, Eq)

-- | Estimate energy consumption based on code characteristics
-- This is a heuristic model - actual measurement would be more accurate
estimateEnergyConsumption :: CodeAnalysisInput -> Double
estimateEnergyConsumption input =
  baseEnergy * complexityFactor * ioFactor * parallelismFactor
  where
    baseEnergy = 0.001  -- Base energy in kWh per execution

    -- Complexity increases energy non-linearly
    complexityFactor =
      1 + (log (fromIntegral (caiComplexity input) + 1) / 10)

    -- I/O is expensive
    ioFactor = 1 + (fromIntegral (caiIOOperations input) * 0.1)

    -- Parallelism can reduce or increase energy depending on efficiency
    parallelismFactor =
      if caiParallelism input > 1
      then 0.8 + (fromIntegral (caiParallelism input) * 0.05)
      else 1.0

-- | Normalize carbon score to 0-100 (100 = lowest carbon)
normalizeScore :: Double -> Double
normalizeScore rawScore
  | rawScore <= 0.001 = 100  -- Excellent
  | rawScore >= 1.0   = 0    -- Very poor
  | otherwise = 100 * (1 - (log rawScore + 6.9) / 6.9)

-- | Identify factors contributing to carbon intensity
identifyFactors :: CodeAnalysisInput -> [Text]
identifyFactors input = concat
  [ ["High complexity increases computation" | caiComplexity input > 20]
  , ["Deep loop nesting may indicate inefficiency" | caiLoopDepth input > 4]
  , ["Many heap allocations increase memory energy" | caiAllocations input > 100]
  , ["Heavy I/O operations are energy-intensive" | caiIOOperations input > 50]
  ]
